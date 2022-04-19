module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

let translate (globals, _ (* objects *), functions) =
  let context = L.global_context () in

  let i32_t      = L.i32_type      context
  and i8_t       = L.i8_type       context
  and i1_t       = L.i1_type       context
  and float_t    = L.double_type   context
  and void_t     = L.void_type     context
  and the_module = L.create_module context "Propeller" in

  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Float -> float_t
    | A.Bool  -> i1_t
    | A.Void  -> void_t
    | _       -> i32_t
  in

  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) = 
      let init = match t with
          A.Float -> L.const_float (ltype_of_typ t) 0.0
        | _ -> L.const_int (ltype_of_typ t) 0 in
      StringMap.add n (L.define_global n init the_module) m
    in
    List.fold_left global_var StringMap.empty globals in

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

    let local_vars =
      let add_formal m (t, n) p = 
        let () = L.set_value_name n p in
	      let local = L.build_alloca (ltype_of_typ t) n builder in
        let _  = L.build_store p local builder in
	      StringMap.add n local m
      in
      let add_local m (t, n) =
	      let local_var = L.build_alloca (ltype_of_typ t) n builder
	      in StringMap.add n local_var m 
      in
      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
                                    (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals in
      
    let lookup n =
      try  StringMap.find n local_vars
      with Not_found -> StringMap.find n global_vars
    in 

    let rec expr builder ((_, e) : sexpr) = match e with
        SIliteral x -> L.const_int i32_t x
      | SFliteral x -> L.const_float float_t x
      | SBliteral x -> L.const_int i1_t (if x then 1 else 0)
      (* | SSLiteral x -> *)
      (* | SLliteral xs -> *)
      | SCall (f, es) -> (match (f, es) with 
                             ("print", [e]) -> L.build_call print_func [| int_format_str ; (expr builder e) |] "print" builder
                           | _ -> let (fdef, fdecl) = StringMap.find f function_decls in
                                  let lles = List.rev (List.map (expr builder) (List.rev es)) in
                                  let result = (match fdecl.styp with 
                                                    A.Void -> ""
                                                  | _ -> f ^ "_result") in
                                  L.build_call fdef (Array.of_list lles) result builder)
                                  
      | SAssign (id, e) ->
          let e' = expr builder e in
          let _ = L.build_store e' (lookup id) builder in
          e'
      (* | SSetprop (o, p, e) -> *)
      | SId id -> L.build_load (lookup id) id builder
      (* | Getprop (o, p) -> *)
      (* | SIndex (id, e) -> *)
      | SBinop (e1, op, e2) ->
          let (t, _) = e1
          and e1' = expr builder e1
          and e2' = expr builder e2 in
          let instr = (match t with
               A.Int -> (match op with
                             A.Add -> L.build_add
                           | A.Sub -> L.build_sub
                           | A.Mlt -> L.build_mul
                           | A.Div -> L.build_sdiv
                           | A.Mod -> raise (Failure "modulo not implemented")
                           | A.Eq  -> L.build_icmp L.Icmp.Eq
                           | A.Neq -> L.build_icmp L.Icmp.Ne
                           | A.Lt  -> L.build_icmp L.Icmp.Slt
                           | A.Leq -> L.build_icmp L.Icmp.Sle
                           | A.Gt  -> L.build_icmp L.Icmp.Sgt
                           | A.Geq -> L.build_icmp L.Icmp.Sge
                           | _ -> raise (Failure "internal error - bad int operator"))
             | A.Float -> (match op with
                               A.Add -> L.build_fadd
                             | A.Sub -> L.build_fsub
                             | A.Mlt -> L.build_fmul
                             | A.Div -> L.build_fdiv 
                             | A.Eq  -> L.build_fcmp L.Fcmp.Oeq
                             | A.Neq -> L.build_fcmp L.Fcmp.One
                             | A.Lt  -> L.build_fcmp L.Fcmp.Olt
                             | A.Leq -> L.build_fcmp L.Fcmp.Ole
                             | A.Gt  -> L.build_fcmp L.Fcmp.Ogt
                             | A.Geq -> L.build_fcmp L.Fcmp.Oge
                             | _ -> raise (Failure "internal error - bad float operator"))
              | A.Bool -> (match op with
                               A.Eq  -> L.build_icmp L.Icmp.Eq
                             | A.Neq -> L.build_icmp L.Icmp.Ne
                             | A.And -> L.build_and
                             | A.Or  -> L.build_or
                             | A.Xor -> raise (Failure "internal error - bad float operator")
                             | _ -> raise (Failure "internal error - bad bool operator"))
              | _ -> raise (Failure ("internal error - bad binary operator type"))) in
          instr e1' e2' "tmp" builder
      | SUnop (op, e) ->
          let (t, _) = e in
          let e' = expr builder e in
          let instr = (match op with
                           A.Neg when t = A.Float -> L.build_fneg
                         | A.Neg when t = A.Int   -> L.build_neg
                         | A.Not when t = A.Bool  -> L.build_not
                         | _ -> raise (Failure "bad unary operator/operand")) in
          instr e' "tmp" builder
      | SParentheses e -> expr builder e
      | SNoexpr -> L.const_int i32_t 0
      | _ -> L.const_int i32_t 0
    in

    let add_terminal builder instr = match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder)
    in

    let rec stmt builder = function (* not yet recursive -- causes warnings *)
        SExpr e -> let _ = expr builder e in builder
      | SReturn e -> 
          let _ = match fdecl.styp with
                      A.Void -> L.build_ret_void builder
                    | _      -> L.build_ret (expr builder e) builder in
          builder
      | SIf (e, s1, elifs, s2) ->

          (* convert elifs to nested SIf *)
          let s2' = (match elifs with
              [] -> s2
            | _  -> 
                let rec transform_elifs = function
                    [(elif_e, elif_s)]     -> SIf(elif_e, elif_s, [], s2)
                  | (elif_e, elif_s) :: es  -> SIf(elif_e, elif_s, [], [transform_elifs es])
                in
                [transform_elifs elifs]) in

          (* merge block, branch to merge block instruction *)
          let merge_bb = L.append_block context "merge" the_function in
          let branch_instr = L.build_br merge_bb in

          (* add instruction builders for a list of statements to the end of a basic block *)
          let build_bb_stmts bb s =
            let build_bb_stmt st =
              stmt (L.builder_at_end context bb) st
            in
            List.map build_bb_stmt s
          in

          (* add terminal to end of list of statements *)
          let rec terminate = function
              [] -> ()
            | (t :: []) -> add_terminal t branch_instr
            | (_ :: ts) -> terminate ts
          in

          (* if *)
          let if_bool = expr builder e in
          let if_bb = L.append_block context "if" the_function in
          let if_builders = build_bb_stmts if_bb s1 in
          let () = terminate if_builders in

          (* else *)
          let else_bb = L.append_block context "else" the_function in
          let else_builders = (match s2' with
              [] -> [stmt (L.builder_at_end context else_bb) (SExpr (A.Int, SNoexpr))]
            | _ ->  build_bb_stmts else_bb s2') in
          let () = terminate else_builders in

          (* build stuff *)
          let _ = L.build_cond_br if_bool if_bb else_bb builder in
          L.builder_at_end context merge_bb

      | SWhile (e, s) ->
          let e_bb = L.append_block context "while" the_function in
          let _    = L.build_br e_bb builder in
          let s_bb = L.append_block context "while_body" the_function in
          let build_one_stmt st =
            stmt (L.builder_at_end context s_bb) st
          in
          let while_builders = List.map build_one_stmt s in
          let rec add_terminals = function
              [] -> ()
            | (t::ts) -> add_terminal t (L.build_br e_bb); add_terminals ts
          in
          let () = add_terminals while_builders in
          let e_builder = L.builder_at_end context e_bb in
          let bool_val = expr e_builder e in
          let merge_bb = L.append_block context "merge" the_function in
          let _ = L.build_cond_br bool_val s_bb merge_bb e_builder in
          L.builder_at_end context merge_bb

      | _ -> let _ = expr builder (A.Int, SIliteral 0) in builder
    in

    let builder = List.fold_left stmt builder fdecl.sbody in

    add_terminal builder (match fdecl.styp with
        t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
