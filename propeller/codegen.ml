module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

let translate ( (*globals, objects*) _, _, functions) =
  let context = L.global_context () in

  let i32_t      = L.i32_type      context
  and i8_t       = L.i8_type       context
  and i1_t       = L.i1_type       context
  and float_t    = L.double_type   context
  and the_module = L.create_module context "Propeller" in

  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Float -> float_t
    | A.Bool  -> i1_t
    | _       -> i32_t
  in

  (*let global_vars : L.llvalue StringMap.t = StringMap.empty in*)

  let print_t : L.lltype = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let print_func : L.llvalue = L.declare_function "printf" print_t the_module in

  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) fdecl.sformals) in
      let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m
    in
    List.fold_left function_decl StringMap.empty functions in

  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

    (*let local_vars = StringMap.empty in

    let lookup n =
      try StringMap.find n local_vars
      with Not_found -> StringMap.find n global_vars
    in*)

    let rec expr builder ((_, e) : sexpr) = match e with
        SIliteral x -> L.const_int i32_t x
      | SFliteral x -> L.const_float float_t x
      | SCall ("print", [e]) -> L.build_call print_func [| int_format_str ; (expr builder e) |] "print" builder
      | SBliteral b -> L.const_int i1_t (if b then 1 else 0)
      | _ -> L.const_int i32_t 0
    in

    let add_terminal builder instr = match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder)
    in

    let (*rec*) stmt builder = function (* not yet recursive -- causes warnings *)
        SExpr e -> let _ = expr builder e in builder
      | SReturn e -> let _ = (* TODO: case for function returning void *)
                       L.build_ret (expr builder e) builder
                     in builder
      | _ -> let _ = expr builder (Int, SIliteral 0) in builder
    in

    let builder = List.fold_left stmt builder fdecl.sbody in

    add_terminal builder (match fdecl.styp with
        t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
