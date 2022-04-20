open Ast
open Sast

module StringMap = Map.Make(String)

let check (globals, objects, functions) =

  (* global variables *)

  let check_binds kind to_check =
    let name_compare (_, n1) (_, n2) =
      compare n1 n2
    in
    let check_it checked binding = match binding with
        (Void, _) -> raise (Failure ("void " ^ kind))
      | (_, n1)   -> match checked with
          ((_, n2) :: _) when n1 = n2 -> raise (Failure ("duplicate " ^ kind))
        | _ -> binding :: checked
    in
    let _ = List.fold_left check_it [] (List.sort name_compare to_check) in
    to_check
  in

  let globals' = check_binds "global" globals in

  (* objects *)

  let check_obj odecl = {
    soname = odecl.oname;
    sprops = check_binds "property" odecl.props;
    sextern = odecl.extern }
  in

  let objects' = List.map check_obj objects in

  let add_obj map odecl = match odecl with
      _ when StringMap.mem odecl.soname map -> raise (Failure "duplicate objdef")
    | _ ->   StringMap.add odecl.soname odecl map
  in

  let object_decls = List.fold_left add_obj StringMap.empty objects' in

  let find_objdecl o =
    try StringMap.find o object_decls
    with Not_found -> raise (Failure ("undefined object " ^ o))
  in

  let get_prop p odecl =
    let f (_, name) =
      name = p
    in
    try  List.find f odecl.sprops
    with Not_found -> raise (Failure ("object type " ^ odecl.soname ^ " has no property " ^ p))
  in
  (* functions *)

  let built_in_decls =
    let add_bind map (name, t) = StringMap.add name
      { typ = Void;
        fname = name;
        formals = [(t, "x")];
        locals = [];
        body = []; } map
    in
    List.fold_left add_bind StringMap.empty [ ("print", Int)] in

  let add_func map fdecl = match fdecl with
      _ when StringMap.mem fdecl.fname built_in_decls -> raise (Failure "already a built-in function")
    | _ when StringMap.mem fdecl.fname map            -> raise (Failure "duplicate function")
    | _ -> StringMap.add fdecl.fname fdecl map
  in

  let function_decls = List.fold_left add_func built_in_decls functions in

  let find_func f =
    try StringMap.find f function_decls
    with Not_found -> raise (Failure ("undefined function " ^ f))
  in

  let _ = find_func "main" in

  let check_function func =

    let check_locals kind to_check =
      let name_compare (_, n1) (_, n2) =
        compare n1 n2
      in
      let check_it checked binding = match binding with
          (Void, _) -> raise (Failure ("void " ^ kind))
        | (Custom o, n1) -> let _ = find_objdecl o in
                            (match checked with
                                ((_, n2) :: _) when n1 = n2 -> raise (Failure ("duplicate " ^ kind))
                              | _ -> binding :: checked)
        | (_, n1)   -> match checked with
            ((_, n2) :: _) when n1 = n2 -> raise (Failure ("duplicate " ^ kind))
          | _ -> binding :: checked
      in
      let _ = List.fold_left check_it [] (List.sort name_compare to_check) in
      to_check
    in

    let formals' = check_binds "formal" func.formals in
    let locals'  = check_locals "local" func.locals in
    (* let iters    = ref [] in *)

    let check_assign lt rt msg =
      if   lt = rt
      then lt
      else raise (Failure msg)
    in

    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
	                               StringMap.empty (globals' @ formals' @ locals') in
    (* let r_symbols = ref symbols in *)

    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undefined identifier " ^ s))
    in

    let rec expr = function
        Iliteral x -> (Int,   SIliteral x)
      | Fliteral x -> (Float, SFliteral x)
      | Bliteral x -> (Bool,  SBliteral x)
      | Sliteral x -> (Str,   SSliteral x)
      | Lliteral xs ->
          let (ty, _) = expr (Array.get xs 0) in
          let eqty e =
            let (tt, _) = expr e in
            tt = ty
          in
          let tocheckedlit = function 
              Iliteral e -> (Int, SIliteral e)
            | Fliteral e -> (Float, SFliteral e)
            | Bliteral e -> (Bool, SBliteral e)
            | Sliteral e -> (Str, SSliteral e)
            | _ -> raise (Failure "invalid literal in list")
          in
          let same = List.fold_left ( = ) true (List.map eqty (Array.to_list xs)) in
          if   not same
          then raise (Failure "unequal list element types")
          else (match ty with
                    Int   -> let sxs = Array.map tocheckedlit xs in
                             (List(Int), SLliteral sxs)
                  | Float -> let sxs = Array.map tocheckedlit xs in
                             (List(Float), SLliteral sxs)
                  | Bool  -> let sxs = Array.map tocheckedlit xs in
                             (List(Bool), SLliteral sxs)
                  | Str   -> let sxs = Array.map tocheckedlit xs in
                             (List(Str), SLliteral sxs)
                  | _     -> raise (Failure "bad list type"))
      | Id id -> (type_of_identifier id, SId id)
      | Getprop (o, p) -> (* (Int, SIliteral 0) *)
          let otype = type_of_identifier o in
          (match otype with
              Custom t ->
                let odecl = find_objdecl t in
                let (pty, _) = get_prop p odecl in
                (pty, SGetprop(o, p))
            | _       -> raise (Failure (o ^ " is not an object")))
      | Index (id, e) ->
          let ty = (match type_of_identifier id with
                       List(t) -> t
                     | _      -> raise (Failure "not a list")) in
          let (t, e') = expr e in
          (match t with
              Int -> (ty, SIndex(id, (t, e')))
            | _   -> raise (Failure "non-int index"))
      | Call (f, es) ->
          let fdecl = find_func f in
          let n_args = List.length fdecl.formals in
          if   List.length es != n_args
          then raise (Failure "nargs")
          else
          let check_call (ft, _) e =
            let (et, e') = expr e in
            (check_assign ft et "argtype", e')
          in
          let es' = List.map2 check_call fdecl.formals es in
          (fdecl.typ, SCall(f, es'))
      | Assign (id, e) ->
          let tid = type_of_identifier id
          and (te, e') = expr e in
          let err_msg = "Illegal assignment" in
          (check_assign tid te err_msg, SAssign(id, (te, e')))
      | Setprop (o, p, e) ->
        let otype = type_of_identifier o in
        (match otype with
            Custom t ->
              let odecl = find_objdecl t in
              let (pt, _) = get_prop p odecl in
              let (et, e') = expr e in
              (check_assign pt et "illegal property assignment", SSetprop(o, p, (et, e')))
          | _        -> raise (Failure (o ^ " is not an object")))
      | Binop (e1, op, e2) ->
          let (t1, e1') = expr e1
          and (t2, e2') = expr e2 in
          let ty = match op with
              Add | Sub | Mlt | Div | Mod when t1 = t2 && t1 = Int   -> Int
            | Add | Sub | Mlt | Div       when t1 = t2 && t1 = Float -> Float
            | Eq  | Neq                   when t1 = t2               -> Bool
            | Lt  | Leq | Gt  | Geq       when t1 = t2 && (t1 = Int || t2 = Float) -> Bool
            | And | Or  | Xor             when t1 = t2 && t1 = Bool -> Bool
            | _ -> raise (Failure "Illegal binary operator") in
          (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Unop (op, e) ->
          let (t, e') = expr e in
          let ty = match op with
              Neg when t == Int || t == Float -> t
            | Not when t == Bool -> Bool
            | _ -> raise (Failure "Illegal unary operator") in
          (ty, SUnop(op, (t, e')))
      | Parentheses e ->
          let (ty, e') = expr e in
          (ty, SParentheses (ty, e'))
      | Noexpr -> (Void, SNoexpr)
    in

    let check_bool_expr e =
      let (t', e') = expr e in
      if   t' != Bool
      then raise (Failure "expected boolean expression")
      else (t', e')
    in

    let check_int_expr e =
      let (t', e') = expr e in
      if   t' != Int
      then raise (Failure "expected int expression")
      else (t', e')
    in

    let rec check_stmt in_loop stmt = match stmt with
        Expr e -> SExpr (expr e)
      | Return e ->
        let (ty, ex) = expr e in
        let _ = check_assign ty func.typ "bad return type" in
        SReturn (ty, ex)
      | If (e, s1, elifs, s2) ->
          let check_elif (elif_e, elif_s) =
            (check_bool_expr elif_e, List.map (check_stmt in_loop) elif_s)
          in
          let elifs' = match elifs with
              [] -> []
            | _  -> List.map check_elif elifs in
          let s2' = match s2 with
              [] -> []
            | _  -> List.map (check_stmt in_loop) s2 in
          SIf (check_bool_expr e, List.map (check_stmt in_loop) s1, elifs', s2')
      (* Need to somehow create local indexing variable *)
      | For (id, e1, e2, s) -> 
          (* StringMap.add id Int !r_symbols; *)
          SFor (id, check_int_expr e1, check_int_expr e2, List.map (check_stmt true) s)
      | While (e, s) -> SWhile (check_bool_expr e, List.map (check_stmt true) s)
      (* | _ -> SExpr (expr (Iliteral 0)) *)
      | Break    -> if not in_loop then raise (Failure "break outside loop") else SBreak
      | Continue -> if not in_loop then raise (Failure "continue outside loop") else SContinue
      | Bind (o, p, f) -> 
          let oty = type_of_identifier o in
          (match oty with
               Custom t ->
                 let odecl = find_objdecl t in
                 let _ = get_prop p odecl in
                 let _ = find_func f in
                 SBind (o, p, f)
             | _ -> raise (Failure (o ^ " is not an object")))
      | Unbind (o, p, f) -> 
          let oty = type_of_identifier o in
          (match oty with
               Custom t ->
                 let odecl = find_objdecl t in
                 let _ = get_prop p odecl in
                 let _ = find_func f in
                 SUnbind (o, p, f)
               | _ -> raise (Failure (o ^ " is not an object")))
    in

    { styp = func.typ;
      sfname = func.fname;
      sformals = formals';
      slocals = locals';
      sbody = List.map (check_stmt false) func.body }
  in

  let functions' = List.map check_function functions in
  (globals', objects', functions')
