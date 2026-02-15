open Tast
open X86_64
open Ast
module StringSet = Set.Make (String)

module CompilationUtils = struct
  let fold_left_concat f = List.fold_left (fun acc x -> acc ++ f x) nop

  let new_label =
    let r = ref 0 in
    fun () ->
      incr r;
      "L_" ^ string_of_int !r
end

module BoolOps = struct
  (* it tells you how are booleans represented in the assembly *)
  let true_value = 1
  let false_value = 0
  let to_asm_value b = if b then true_value else false_value

  (* negates boolean value from the rax register *)
  let generate_negation_code =
    xorq (imm (true_value lxor false_value)) (reg rax)
end

module Constants = struct
  (* printf format strings *)
  let format_int_label = ".Sint"
  let format_string_label = ".Sstr"

  (* custom print values *)
  let format_true_label = ".Strue"
  let format_false_label = ".Sfalse"
  let format_nil_label = ".Snil"
  let format_false_value = "false"
  let format_true_value = "true"
  let format_nil_value = "<nil>"

  (* Prefix for string constant labels *)
  let string_label_prefix = ".SStrConst"

  (* builtin functions *)
  let function_printf_label = "printf_"
  let function_malloc_label = "malloc_"
  let function_calloc_label = "calloc_"
end

module SizeConstants = struct
  let standard_field_size_bytes = 8
end

(* String table to manage string constants globally *)
module StringTable = struct
  type t = { table : (string, string) Hashtbl.t; mutable counter : int }

  let create () = { table = Hashtbl.create 16; counter = 0 }

  (* Global instance - always initialized *)
  let instance : t ref = ref (create ())
  let reset () = instance := create ()

  (* Add a string and return its label *)
  let add (s : string) : string =
    let inst = !instance in
    if Hashtbl.mem inst.table s then Hashtbl.find inst.table s
    else begin
      let label = Constants.string_label_prefix ^ string_of_int inst.counter in
      Hashtbl.add inst.table s label;
      inst.counter <- inst.counter + 1;
      label
    end

  (* Lookup a string's label *)
  let lookup (s : string) : string option = Hashtbl.find_opt !instance.table s

  (* Get all strings as (string, label) pairs *)
  let get_all () : (string * string) list =
    Hashtbl.fold (fun s lbl acc -> (s, lbl) :: acc) !instance.table []

  (* Check if a string exists *)
  let mem (s : string) : bool = Hashtbl.mem !instance.table s
end

(* Data section generation *)
module Data = struct
  (* Traverse entire Tast tree and collect string constants *)
  let rec visit_expr (e : Tast.expr) : unit =
    match e.expr_desc with
    | TEconstant (Cstring s) -> ignore (StringTable.add s)
    | TEprint exprs -> List.iter visit_expr exprs
    | TEblock exprs -> List.iter visit_expr exprs
    | TEbinop (_, e1, e2) ->
        visit_expr e1;
        visit_expr e2
    | TEunop (_, e) -> visit_expr e
    | TEcall (_, exprs) -> List.iter visit_expr exprs
    | TEassign (lhs, rhs) ->
        List.iter visit_expr lhs;
        List.iter visit_expr rhs
    | TEif (e1, e2, e3) ->
        visit_expr e1;
        visit_expr e2;
        visit_expr e3
    | TEfor (e1, e2) ->
        visit_expr e1;
        visit_expr e2
    | TEdot (e, _) -> visit_expr e
    | TEreturn exprs -> List.iter visit_expr exprs
    | _ -> ()

  let visit_decl (td : Tast.tdecl) : unit =
    match td with TDfunction (_, body) -> visit_expr body | _ -> ()

  (* Collect all strings from the program *)
  let collect_strings (tfile : Tast.tfile) : unit =
    StringTable.reset ();
    (* Clear any previous state *)
    List.iter visit_decl tfile

  (* Generate format constants *)
  let generate_format_constants () : X86_64.data =
    let open X86_64 in
    label Constants.format_int_label
    ++ string "%d"
    ++ label Constants.format_string_label
    ++ string "%s"
    ++ label Constants.format_true_label
    ++ string Constants.format_true_value
    ++ label Constants.format_false_label
    ++ string Constants.format_false_value
    ++ label Constants.format_nil_label
    ++ string Constants.format_nil_value

  (* Generate string constant data *)
  let generate_string_constants () : X86_64.data =
    let open X86_64 in
    let strings = StringTable.get_all () in
    List.fold_left
      (fun acc (s, lbl) -> acc ++ label lbl ++ string s)
      nop strings

  (* Generate complete data section *)
  let generate_data_section (tfile : Tast.tfile) : X86_64.data =
    let open X86_64 in
    (* First, collect all strings *)
    collect_strings tfile;

    (* Generate both user strings and format constants *)
    generate_string_constants () ++ generate_format_constants ()
end

module Allocation = struct
  (* Calculates offset of fields and size of entire structure *)
  let allocate_all_structures (tfile : Tast.tfile) : unit =
    let allocated_structures = ref StringSet.empty in

    let sizeof (t : typ) : int =
      match t with
      | Tstruct s -> s.s_size
      | _ -> SizeConstants.standard_field_size_bytes
    in

    (* recursively calculates size of structures and its fields *)
    let rec allocate_structure (s : structure) : unit =
      if StringSet.mem s.s_name !allocated_structures then ()
      else begin
        let offset = ref 0 in

        (* process all fields *)
        List.iter
          (fun f ->
            match f.f_typ with
            | Tstruct nested -> allocate_structure nested
            | _ ->
                ();

                f.f_ofs <- !offset;
                offset := !offset + sizeof f.f_typ)
          s.s_list;

        s.s_size <- !offset;
        allocated_structures := StringSet.add s.s_name !allocated_structures
      end
    in

    List.iter
      (fun td -> match td with TDstruct s -> allocate_structure s | _ -> ())
      tfile;

    ()

  (* traverses program tree and allocates variables on the updates offsets *)
  (* returns total size of the stack *)
  let allocate_variables (body : expr) : int =
    let offset = ref 0 in

    let rec visit_expr (e : Tast.expr) : unit =
      match e.expr_desc with
      | TEvars var_list ->
          List.iter
            (fun v ->
              offset := !offset - 8;
              v.v_ofs <- !offset)
            var_list
      | TEbinop (_, e1, e2) ->
          visit_expr e1;
          visit_expr e2
      | TEunop (_, e) -> visit_expr e
      | TEcall (_, exprs) -> List.iter visit_expr exprs
      | TEdot (e, _) -> visit_expr e
      | TEassign (lhs, rhs) ->
          List.iter visit_expr lhs;
          List.iter visit_expr rhs
      | TEif (e1, e2, e3) ->
          visit_expr e1;
          visit_expr e2;
          visit_expr e3
      | TEreturn exprs -> List.iter visit_expr exprs
      | TEblock exprs -> List.iter visit_expr exprs
      | TEfor (e1, e2) ->
          visit_expr e1;
          visit_expr e2
      | TEprint exprs -> List.iter visit_expr exprs
      | TEincdec (e, _) -> visit_expr e
      | _ -> ()
    in

    visit_expr body;

    - !offset

  let allocate_function_params (fn : function_) : unit =
    (* parameters are on the other side of rbp *)
    (* that's why they have positive offsets *)
    (* initial value is 16 because we skip return address and previous rbp *)
    let offset = ref 16 in
    List.iter
      (fun param ->
        param.v_ofs <- !offset;
        offset := !offset + 8)
      fn.fn_params;
    ()

  (* calculates offsets for all the variables and parameters *)
  let allocate_function (fn : function_) (body : expr) : int =
    allocate_function_params fn;

    (* allocate local variables *)
    allocate_variables body
end

module Compilation = struct
  let constant (e : Tast.constant) : text =
    match e with
    | Cstring s ->
        let lbl = StringTable.add s in
        leaq (lab lbl) rax
    | Cbool b -> movq (imm (BoolOps.to_asm_value b)) (reg rax)
    | Cint i -> movq (imm64 i) (reg rax)

  let unop (compile_expr : expr -> text) (op : Tast.unop) (e : Tast.expr) : text
      =
    match op with
    | Uneg -> compile_expr e ++ negq (reg rax)
    | Unot -> compile_expr e ++ BoolOps.generate_negation_code
    | Uamp -> (
        match e.expr_desc with
        | TEident v -> leaq (ind ~ofs:v.v_ofs rbp) rax
        | TEdot (ee, field) ->
            compile_expr ee ++ leaq (ind ~ofs:field.f_ofs rax) rax
        | _ -> failwith "Cannot take address of non-variable expression")
    | Ustar -> compile_expr e ++ movq (ind rax) (reg rax)

  let print (compile_expr : expr -> text) (expr_list : expr list) : text =
    CompilationUtils.fold_left_concat
      (fun e ->
        compile_expr e
        ++
        match e.expr_typ with
        (* first argument in rdi (format string), second in rsi (value)*)
        | Tstring ->
            leaq (lab Constants.format_string_label) rdi
            ++ movq (reg rax) (reg rsi)
            ++ call Constants.function_printf_label
        | Tint ->
            leaq (lab Constants.format_int_label) rdi
            ++ movq (reg rax) (reg rsi)
            ++ call Constants.function_printf_label
        | Tbool ->
            (* rax has 0 or 1 *)

            (* labels for branches *)
            let lbl_true = CompilationUtils.new_label () in
            let lbl_end = CompilationUtils.new_label () in

            (* compare bool with value 0 *)
            cmpq (imm 0) (reg rax)
            ++ jne lbl_true
            ++
            (* we want to put address of proper string to the rax register *)
            (* case false *)
            leaq (lab Constants.format_false_label) rax
            ++ jmp lbl_end
            ++
            (* case true *)
            label lbl_true
            ++ leaq (lab Constants.format_true_label) rax
            ++
            (* rax has address of string to print *)
            label lbl_end
            ++ leaq (lab Constants.format_string_label) rdi
            ++ movq (reg rax) (reg rsi)
            ++ call Constants.function_printf_label
        | _ -> failwith "Unsupported type for print")
      expr_list

  let block (compile_expr : expr -> text) (expr_list : expr list) : text =
    CompilationUtils.fold_left_concat compile_expr expr_list

  let rec compile_expr (e : expr) : text =
    match e.expr_desc with
    | TEconstant const -> constant const
    | TEunop (op, e) -> unop compile_expr op e
    | TEprint expr_list -> print compile_expr expr_list
    | TEblock expr_list -> block compile_expr expr_list
    | _ -> failwith "Unsupported expression type"

  let compile_function (fn : function_) (body : expr) : text =
    let local_stack_size = Allocation.allocate_function fn body in

    label fn.fn_name
    ++ pushq (reg rbp)
    ++ movq (reg rsp) (reg rbp)
    ++ compile_expr body ++ leave ++ ret
end
