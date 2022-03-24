open Ast
open Sast

module StringMap = Map.Make(String)

let check (globals, objects, functions) =
  let check_binds kind to_check =
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

    let check_assign lt rt msg =
      if   lt = rt
      then lt
      else raise (Failure msg)
    in

    let symbols = List.fold_left (fun map (t, name) -> StringMap.add name t map) StringMap.empty (globals' @ formals' @ locals') in

    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure "undef")
    in

    let rec expr = function
        Iliteral x -> (Int, SIliteral x)
      | Fliteral x -> (Float, SFliteral x)
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
      | _ -> (Int, SIliteral 0)
    in

    let rec check_stmt = function
        Expr e -> SExpr (expr e)
      | _ -> SExpr (expr (Iliteral 0))
    in

    { styp = func.typ;
      sfname = func.fname;
      sformals = formals';
      slocals = locals';
      sbody = List.map check_stmt func.body }
  in

  let functions' = List.map check_function functions in
  (globals', objects, functions')
