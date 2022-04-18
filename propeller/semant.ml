open Ast
open Sast

module StringMap = Map.Make(String)

let check (globals, objects, functions) =
  let check_binds (*kind*) _ to_check =
    let name_compare (_, n1) (_, n2) =
      compare n1 n2
    in
    let check_it checked binding = match binding with
        (Void, _) -> raise (Failure "void")
      | (_, n1)   -> match checked with
          ((_, n2) :: _) when n1 = n2 -> raise (Failure "dup")
        | _ -> binding :: checked
    in
    let _ = List.fold_left check_it [] (List.sort name_compare to_check) in
    to_check
  in

  let globals' = check_binds "global" globals in

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
      _ when StringMap.mem fdecl.fname built_in_decls -> raise (Failure "built-in")
    | _ when StringMap.mem fdecl.fname map            -> raise (Failure "dup")
    | _ -> StringMap.add fdecl.fname fdecl map
  in

  let function_decls = List.fold_left add_func built_in_decls functions in

  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure "undef")
  in

  let _ = find_func "main" in

  let check_function func =
    let formals' = check_binds "formal" func.formals in
    let locals'  = check_binds "local" func.locals in
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
      with Not_found -> raise (Failure "undef")
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
          let same = List.fold_left ( = ) true (List.map eqty (Array.to_list xs)) in
          if   not same
          then raise (Failure "unequal list element types")
          else (match ty with
                    Int   -> let sxs = Array.map (fun (Iliteral e) -> (Int, SIliteral e)) xs in
                             (List(Int), SLliteral sxs)
                  | Float -> let sxs = Array.map (fun (Fliteral e) -> (Float, SFliteral e)) xs in
                             (List(Float), SLliteral sxs)
                  | Bool  -> let sxs = Array.map (fun (Bliteral e) -> (Bool, SBliteral e)) xs in
                             (List(Bool), SLliteral sxs)
                  | Str   -> let sxs = Array.map (fun (Sliteral e) -> (Str, SSliteral e)) xs in
                             (List(Str), SLliteral sxs)
                  | _     -> raise (Failure "bad list type"))
      | Id id -> (type_of_identifier id, SId id)
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
      | _ -> raise (Failure "bad expr")
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

    let rec check_stmt = function
        Expr e -> SExpr (expr e)
      | Return e -> SReturn (expr e)
      | If (e, s1, elifs, s2) ->
          let check_elif (elif_e, elif_s) =
            (check_bool_expr elif_e, List.map check_stmt elif_s)
          in
          let elifs' = match elifs with
              [] -> []
            | _  -> List.map check_elif elifs in
          let s2' = match s2 with
              [] -> []
            | _  -> List.map check_stmt s2 in
          SIf (check_bool_expr e, List.map check_stmt s1, elifs', s2')
      (* Need to somehow create local indexing variable *)
      | For (id, e1, e2, s) -> 
          (* StringMap.add id Int !r_symbols; *)
          SFor (id, check_int_expr e1, check_int_expr e2, List.map check_stmt s)
      | While (e, s) -> SWhile (check_bool_expr e, List.map check_stmt s)
      (* | _ -> SExpr (expr (Iliteral 0)) *)
      | Break    -> SBreak
      | Continue -> SContinue
      | _ -> raise (Failure "bad stmt")
    in

    { styp = func.typ;
      sfname = func.fname;
      sformals = formals';
      slocals = locals';
      sbody = List.map check_stmt func.body }
  in

  let functions' = List.map check_function functions in
  (globals', objects, functions')