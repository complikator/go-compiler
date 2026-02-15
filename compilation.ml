open Tast
open X86_64
open Ast

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
    | _ -> assert false (* TODO: compile the rest *)

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
        | _ -> assert false)
      expr_list

  let block (compile_expr : expr -> text) (expr_list : expr list) : text =
    CompilationUtils.fold_left_concat compile_expr expr_list

  let rec compile_expr (e : expr) : text =
    match e.expr_desc with
    | TEconstant const -> constant const
    | TEunop (op, e) -> unop compile_expr op e
    | TEprint expr_list -> print compile_expr expr_list
    | TEblock expr_list -> block compile_expr expr_list
    | _ -> nop (* TODO: compile other expressions *)

  let compile_function (fn : function_) (body : expr) : text =
    label fn.fn_name
    ++ pushq (reg rbp)
    ++ movq (reg rsp) (reg rbp)
    ++ compile_expr body ++ leave ++ ret
end
