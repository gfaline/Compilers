(* Authors: Isra Ali, Gwendolyn Edgar, Randy Price, Chris Xiong *)
module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

let translate (globals, objects, functions) =
  let context = L.global_context () in

  let i32_t      = L.i32_type      context
  and i8_t       = L.i8_type       context
  and i1_t       = L.i1_type       context
  and float_t    = L.double_type   context
  and void_t     = L.void_type     context
  and the_module = L.create_module context "Propeller" in

  (* let objdef_strs = String.concat "\n" (List.map (fun o -> o.soname) objects) in *)

  let is_external = function
      A.Custom t ->
        let objdef = List.find (fun o -> o.soname = t) objects in
        objdef.sextern
    | _ -> false
  in
  
  let rec ltype_of_typ = function
      A.Int   -> i32_t
    | A.Float -> float_t
    | A.Bool  -> i1_t
    | A.Str ->  L.pointer_type (L.i8_type (context)) 
    | A.Void  -> void_t
    | A.Custom t ->
        let objdef = List.find (fun o -> o.soname = t) objects in
        let ptys = List.map fst objdef.sprops in
        let ltys = Array.of_list (List.map ltype_of_typ ptys) in
        if objdef.sextern then i32_t else L.struct_type context ltys
    | A.List(t) -> L.pointer_type (ltype_of_typ t)
    | _       -> i32_t
  in

  (* indices for struct getelementpointer *)
  let get_obj_gep_idx o p =
    let obj_geps = 
      let add_obj m odecl =
        let rec build_pmap n = function
            []         -> StringMap.empty
          | (_, p)::ps -> StringMap.add p n (build_pmap (n + 1) ps)
        in
        let pmap = build_pmap 0 odecl.sprops in
        StringMap.add odecl.soname pmap m
      in
      List.fold_left add_obj StringMap.empty objects in
    StringMap.find p (StringMap.find o obj_geps)
  in

  (* globals *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) = 
      let init = match t with
          A.Float -> L.const_float (ltype_of_typ t) 0.0
        | _ -> L.const_int (ltype_of_typ t) 0 in
      StringMap.add n (L.define_global n init the_module) m
    in
    List.fold_left global_var StringMap.empty globals in

  (* functions *)
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

  (* global symbols *)
  let gsyms = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
                             StringMap.empty globals in


  (* ================ FUNCTION BODY ================ *)
  let build_function_body fdecl =

    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    let str_format_str = L.build_global_stringptr "%s" "fmt" builder in 
    let float_format_str = L.build_global_stringptr "%f\n" "fmt" builder in

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
    
    (* type of symbols *)
    let fsyms = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
                                 StringMap.empty fdecl.sformals in
    let lsyms = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
	                               StringMap.empty fdecl.slocals in

    let type_of_identifier id =
      try StringMap.find id lsyms
      with Not_found -> 
        try StringMap.find id fsyms
        with Not_found -> 
          try StringMap.find id gsyms
          with Not_found -> raise (Failure ("Internal error - undefined identifier " ^ id))
    in
    
    let type_of_prop o p = 
      let objpropt = 
        let add_obj m odecl =
          let rec build_tmap = function
              []         -> StringMap.empty
            | (t, p)::ps -> StringMap.add p t (build_tmap ps)
          in
          let tmap = build_tmap odecl.sprops in
          StringMap.add odecl.soname tmap m
        in
        List.fold_left add_obj StringMap.empty objects in
      StringMap.find p (StringMap.find o objpropt)
    in

    (* get name of object type (objdef <type>) *)
    let type_of_obj o = match type_of_identifier o with
        A.Custom t -> t
      | _ -> raise (Failure (o ^ " is not an object"))
    in
    
    let extcreatef_t : L.lltype = L.function_type i32_t [| |] in
    
    let build_extern_local_init builder = 
      let init_extern_local builder (t, n) =
        if is_external (type_of_identifier n) then
          match t with
              A.Custom t ->
                let create_func : L.llvalue = L.declare_function ("object_new_" ^ t) extcreatef_t the_module in
                let r = L.build_call create_func [| |] "created" builder in
                let _ = L.build_store r (StringMap.find n local_vars) builder in
              builder
            | _ -> raise (Failure ("internal codegen error"))
        else builder
      in List.fold_left init_extern_local builder fdecl.slocals
    in
    
    (* ================ PROPERTY BINDING MAP ================ *)
    (* map of properties and their bound functions : string -> string list
       <var_name>__<prop_name> -> [<function names>] *)
    let bind_map = 
      (* names of local objects *)
      let local_obj_names =
        let is_obj = function
            (A.Custom _, _) -> true
          | _ -> false
        in
        let local_obj_vars = List.filter is_obj fdecl.slocals in
        List.map snd local_obj_vars in

      (* make keys for property-function bindings map *)
      let bind_map_keys =
        let rec make_bind_map_keys = function
            [] -> []
          | o::os -> let props =
                        let get_prop_names_of_var v =
                          let objdef = type_of_obj v in
                          let odecl = try List.find (fun od -> od.soname = objdef) objects
                                      with Not_found -> raise (Failure "can't find object") in
                          let oprops = odecl.sprops in
                          List.map snd oprops
                        in
                        get_prop_names_of_var o in
                        let make_key p =
                          o ^ "__" ^ p
                        in
                        (List.map make_key props) @ (make_bind_map_keys os)
        in
        make_bind_map_keys local_obj_names in

      (* let bind_map_keys = make_bind_map_keys local_obj_names in *)
      let make_empty_lists m k =
        StringMap.add k [] m
      in
      ref (List.fold_left make_empty_lists StringMap.empty bind_map_keys) in

    (* add binding to property*)
    let add_obj_bind o p f =
      let k = (o ^ "__" ^ p) in
      let fs = StringMap.find k !bind_map in
      if List.mem f fs
      then raise (Failure ("function " ^ f ^ " is already bound to " ^ o ^ "." ^ p))
      else
        let new_fs = f :: fs in
        let new_m = StringMap.add k new_fs !bind_map in
        bind_map := new_m
    in

    (* remove binding from property *)
    let rem_obj_bind o p f =
      let k = (o ^ "__" ^ p) in
      let fs = StringMap.find k !bind_map in
      if List.mem f fs
      then
        let new_fs = List.filter (fun n -> n <> f) fs in
        let new_m = StringMap.add k new_fs !bind_map in
        bind_map := new_m
      else raise (Failure ("function " ^ f ^ " is not bound to " ^ o ^ "." ^ p))
    in

    (* get a property's bound functions *)
    let get_bound_funcs o p =
      let k = (o ^ "__" ^ p) in
      StringMap.find k !bind_map
    in

    (* ================ EXPRESSION BUILDER ================ *)
    let rec expr builder ((_, e) : sexpr) = match e with
        SIliteral x -> L.const_int i32_t x
      | SFliteral x -> L.const_float float_t x
      | SBliteral x -> L.const_int i1_t (if x then 1 else 0)
      | SSliteral x ->  L.build_global_stringptr x "str" builder
      | SLliteral xs -> 
        let (x, _) = Array.get xs 0 in
        let allocate = L.build_array_alloca (ltype_of_typ x) (L.const_int i32_t (Array.length xs)) "tmp_list" builder in
        let build_list x i arr =
          let gep_ptr = L.build_gep arr [| L.const_int i32_t i |] "list" builder in
          let _ = L.build_store (expr builder x) gep_ptr builder in 
          i + 1    
        in
        let _ =  Array.fold_left (fun y el -> build_list el y allocate) 0 (Array.of_list (List.rev (Array.to_list xs))) in 
        allocate
      | SCall (f, es) -> (match (f, es) with
            ("print", [e]) | ("printb", [e]) ->
              L.build_call print_func [| int_format_str ; (expr builder e) |] "print" builder
          | ("prints", [e]) -> L.build_call print_func [| str_format_str ; (expr builder e) |] "print" builder
          | ("printf", [e]) -> L.build_call print_func [| float_format_str ; (expr builder e) |] "print" builder
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
      | SSetprop (o, p, e) ->
        if is_external (type_of_identifier o)
        then
          raise (Failure "Property assignment to external objects is unimplemented")
        else
          let e' = expr builder e in
          let objtype = type_of_obj o in
          let idx = get_obj_gep_idx objtype p in
          let id = (o ^ "__" ^ p) in
          let tmp = L.build_struct_gep (lookup o) idx id builder in
          let _ = L.build_store e' tmp builder in
          let fs = get_bound_funcs o p in
          let call_bound_func f =
            expr builder (A.Void, SCall(f, [e; e]))
          in
          let _ = List.map call_bound_func fs in
          e'
      | SId id -> L.build_load (lookup id) id builder
      | SGetprop (o, p) ->
        if is_external (type_of_identifier o)
        then
          let oty = type_of_obj o in
          let pty = type_of_prop (type_of_obj o) p in
          let extgetf_t : L.lltype  = L.function_type (ltype_of_typ pty) [| i32_t |] in
          let get_func  : L.llvalue = L.declare_function ("object_prop_get_" ^ oty ^ "_" ^ p) extgetf_t the_module in
          L.build_call get_func [| (lookup o) |] "get_result" builder
        else
          let objtype = type_of_obj o in
          let idx = get_obj_gep_idx objtype p in
          let tmp = L.build_struct_gep (lookup o) idx (o ^ "__" ^ p) builder in
          L.build_load tmp (o ^ "__" ^ p) builder
      | SIndex (id, e) ->
        let id' = lookup id  in
        let indx = expr builder e in
        let head_ptr = L.build_load id' id builder in
        let elem_ptr  = L.build_gep head_ptr [|indx|] "p" builder in
        L.build_load elem_ptr "tmp" builder
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
                           | A.Mod -> L.build_srem
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
    in

    let add_terminal builder instr = match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder)
    in

    (* ================ STATEMENT BUILDER ================ *)
    let rec stmt loop_start loop_after builder = function
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
                  | [] -> raise (Failure "semant internal error")
                in
                [transform_elifs elifs]) in

          (* merge block, branch to merge block instruction *)
          let merge_bb = L.append_block context "merge" the_function in
          let branch_instr = L.build_br merge_bb in

          (* add instruction builders for a list of statements to the end of a basic block *)
          let build_bb_stmts bb s =
            let build_bb_stmt st =
              stmt loop_start loop_after (L.builder_at_end context bb) st
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
              [] -> [stmt loop_start loop_after (L.builder_at_end context else_bb) (SExpr (A.Int, SNoexpr))]
            | _ ->  build_bb_stmts else_bb s2') in
          let () = terminate else_builders in

          (* build stuff *)
          let _ = L.build_cond_br if_bool if_bb else_bb builder in
          L.builder_at_end context merge_bb

      | SWhile (e, s) ->
          let e_bb = L.append_block context "while" the_function in
          let _    = L.build_br e_bb builder in
          let s_bb = L.append_block context "while_body" the_function in
          let merge_bb = L.append_block context "merge" the_function in
          let while_builder = List.fold_left (stmt (Some e_bb) (Some merge_bb))
                              (L.builder_at_end context s_bb) s in
          let () = add_terminal while_builder (L.build_br e_bb) in
          let e_builder = L.builder_at_end context e_bb in
          let bool_val = expr e_builder e in
          let _ = L.build_cond_br bool_val s_bb merge_bb e_builder in
          L.builder_at_end context merge_bb
      | SFor (id, e1, e2, s) ->
          let b = stmt loop_start loop_after builder (SExpr (A.Int, SAssign(id, e1))) in
          let id_sexpr = (A.Int, SId id) in
          let cmp_sexpr = (A.Bool, SBinop(id_sexpr, A.Leq, e2)) in
          let int1_sexpr = (A.Int, SIliteral 1) in
          let add_sexpr = (A.Int, SBinop (id_sexpr, A.Add, int1_sexpr)) in
          let inc_sexpr = (A.Int, SAssign(id, add_sexpr)) in
          let inc_stmt = SExpr inc_sexpr in
          let while_stmts = s @ [inc_stmt] in
          stmt loop_start loop_after b (SWhile (cmp_sexpr, while_stmts))
      | SContinue ->
          (match loop_start with
            (Some bb) -> let _ = L.build_br bb builder in
                           builder
          | None -> raise (Failure "semant internal error"))
      | SBreak ->
          (match loop_after with
            (Some bb) -> let _ = L.build_br bb builder in
                           builder
          | None -> raise (Failure "semant internal error"))
      | SBind(o, p, f) ->
        if is_external (type_of_identifier o)
        then
          let oty = type_of_obj o in
          let pty = type_of_prop (type_of_obj o) p in
          let extboundfuncp_t : L.lltype = L.pointer_type (L.function_type void_t [| ltype_of_typ pty; ltype_of_typ pty |]) in
          let extbindf_t     : L.lltype = L.function_type void_t [| i32_t; extboundfuncp_t |] in
          let bind_func : L.llvalue = L.declare_function ("object_prop_bind_" ^ oty ^ "_" ^ p) extbindf_t the_module in
          let ov = L.build_load (lookup o) o builder in
          let _ = L.build_call bind_func [| ov; fst (StringMap.find f function_decls) |] "" builder in
          builder
        else let _ = add_obj_bind o p f in builder
      | SUnbind(o, p, f) ->
        if is_external (type_of_identifier o)
        then
          let oty = type_of_obj o in
          let pty = type_of_prop (type_of_obj o) p in
          let extboundfuncp_t : L.lltype = L.pointer_type (L.function_type void_t [| ltype_of_typ pty; ltype_of_typ pty |]) in
          let extbindf_t     : L.lltype = L.function_type void_t [| i32_t; extboundfuncp_t |] in
          let unbind_func : L.llvalue = L.declare_function ("object_prop_unbind_" ^ oty ^ "_" ^ p) extbindf_t the_module in
          let ov = L.build_load (lookup o) o builder in
          let _ = L.build_call unbind_func [| ov; fst (StringMap.find f function_decls) |] "" builder in
          builder
        else let _ = rem_obj_bind o p f in builder
    in

    let b = build_extern_local_init builder in
    let builder = List.fold_left (stmt None None) b fdecl.sbody in

    add_terminal builder (match fdecl.styp with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
