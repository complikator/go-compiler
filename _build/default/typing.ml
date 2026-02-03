(** Static type checking of Mini Go programs (TODO) *)

open Format
open Lib
open Ast
open Tast
open Utils
open Graph
open Return_check
open Unused
module StringSet = Set.Make (String)

let debug = ref false
let fmt_print_used = ref false
let dummy_loc = (Lexing.dummy_pos, Lexing.dummy_pos)

exception Error of Ast.location * string

(** use this function to report errors; it is a printf-like function, eg.

    errorm ~loc "bad arity %d for function %s" n f *)
let errorm ?(loc = dummy_loc) f =
  Format.kasprintf (fun s -> raise (Error (loc, s))) ("@[" ^^ f ^^ "@]")

(** use this function to create variable, so that they all have a unique id if
    field `v_id` *)
let new_var : string -> Ast.location -> typ -> var =
  let id = ref 0 in
  fun x loc ty ->
    incr id;
    {
      v_name = x;
      v_id = !id;
      v_loc = loc;
      v_typ = ty;
      v_used = false;
      v_addr = false;
      v_ofs = -1;
    }

type struct_env = (string, structure) Hashtbl.t
type func_env = (string, function_) Hashtbl.t
type var_env = (string, var) Hashtbl.t list

let empty_env : var_env = [ Hashtbl.create 10 ]
let push_scope (env : var_env) : var_env = Hashtbl.create 10 :: env

let pop_scope (env : var_env) : var_env =
  match env with
  | [] -> errorm "cannot pop from empty variable environment"
  | _ :: rest -> rest

(* adds to the current scope *)
let add_var (env : var_env) (v : var) : unit =
  match env with
  | [] -> errorm "cannot add variable to empty variable environment"
  | scope :: _ -> Hashtbl.add scope v.v_name v

(* tries to get variable from current scope *)
let find_opt_var_current_scope (env : var_env) (name : string) : var option =
  match env with [] -> None | scope :: _ -> Hashtbl.find_opt scope name

(* tries to get variable from all scopes *)
let rec find_opt_var_global (env : var_env) (name : string) : var option =
  match env with
  | [] -> None
  | scope :: rest -> (
      match Hashtbl.find_opt scope name with
      | Some v -> Some v
      | None -> find_opt_var_global rest name)

let rec ptyp_to_typ ?(currently_resolving = StringSet.empty)
    (struct_env : struct_env) (pt : ptyp) : typ =
  match pt with
  | PTident id when id.id = "int" -> Tint
  | PTident id when id.id = "bool" -> Tbool
  | PTident id when id.id = "string" -> Tstring
  | PTident id -> (
      match Hashtbl.find_opt struct_env id.id with
      | Some s -> Tstruct s
      | None ->
          errorm ~loc:id.loc "undefined structure: %s used as a type" id.id)
  | PTptr pt' -> Tptr (ptyp_to_typ struct_env pt')

let build_structure (struct_env : struct_env) (s : pstruct) : structure =
  let fields_table = Hashtbl.create (List.length s.ps_fields) in

  let structure =
    { s_name = s.ps_name.id; s_fields = fields_table; s_list = []; s_size = 0 }
  in

  let fields_list =
    List.map
      (fun (fname, ftyp) ->
        let field =
          { f_name = fname.id; f_typ = ptyp_to_typ struct_env ftyp; f_ofs = 0 }
        in

        Hashtbl.add structure.s_fields fname.id field;

        field)
      s.ps_fields
  in
  structure.s_list <- fields_list;
  structure

let build_function (struct_env : struct_env) (f : pfunc) : function_ =
  let var_list =
    List.map
      (fun (ident, ptyp) ->
        let param_type = ptyp_to_typ struct_env ptyp in
        let v = new_var ident.id ident.loc param_type in
        v)
      f.pf_params
  in

  {
    fn_name = f.pf_name.id;
    fn_params = var_list;
    fn_typ = List.map (ptyp_to_typ struct_env) f.pf_typ;
  }

let is_lvalue (e : pexpr) : bool =
  match e.pexpr_desc with
  | PEident _ -> true
  | PEdot (_, _) -> true
  | PEunop (_, _) -> true
  | _ -> false

let rec typecheck_expr (struct_env : struct_env) (func_env : func_env)
    (var_env : var_env) (expected_return_type : typ list) (e : pexpr) : expr =
  match e.pexpr_desc with
  | PEskip -> { expr_desc = TEskip; expr_typ = Tmany [] }
  | PEconstant c ->
      let typ =
        match c with Cbool _ -> Tbool | Cint _ -> Tint | Cstring _ -> Tstring
      in
      { expr_desc = TEconstant c; expr_typ = typ }
  | PEbinop (op, e1, e2) ->
      let te1 =
        typecheck_expr struct_env func_env var_env expected_return_type e1
      in
      let te2 =
        typecheck_expr struct_env func_env var_env expected_return_type e2
      in

      let t1 = te1.expr_typ in
      let t2 = te2.expr_typ in

      let result_type =
        match op with
        | Badd | Bsub | Bmul | Bdiv | Bmod ->
            if t1 = Tint && t2 = Tint then Tint
            else
              errorm ~loc:e.pexpr_loc
                "operator %s requires two integers%s, got %s and %s"
                (string_of_binop op)
                (if op = Badd then " (or two strings)" else "")
                (string_of_typ t1) (string_of_typ t2)
        | Blt | Ble | Bgt | Bge ->
            if t1 = Tint && t2 = Tint then Tbool
            else
              errorm ~loc:e.pexpr_loc
                "operator %s requires two integers, got %s and %s"
                (string_of_binop op) (string_of_typ t1) (string_of_typ t2)
        | Beq | Bne ->
            if types_equal t1 t2 && not (t1 = Tnil && t2 = Tnil) then Tbool
            else
              errorm ~loc:e.pexpr_loc
                "operator %s requires compatible types, got %s and %s"
                (string_of_binop op) (string_of_typ t1) (string_of_typ t2)
        | Band | Bor ->
            if t1 = Tbool && t2 = Tbool then Tbool
            else
              errorm ~loc:e.pexpr_loc
                "operator %s requires two booleans, got %s and %s"
                (string_of_binop op) (string_of_typ t1) (string_of_typ t2)
      in

      { expr_desc = TEbinop (op, te1, te2); expr_typ = result_type }
  | PEunop (op, e) ->
      let te =
        typecheck_expr struct_env func_env var_env expected_return_type e
      in

      let t = te.expr_typ in

      let result_type =
        match op with
        | Uneg ->
            if t = Tint then Tint
            else
              errorm ~loc:e.pexpr_loc "unary - requires int, got %s"
                (string_of_typ t)
        | Unot ->
            if t = Tbool then Tbool
            else
              errorm ~loc:e.pexpr_loc "unary ! requires bool, got %s"
                (string_of_typ t)
        | Uamp ->
            if not (is_lvalue e) then
              errorm ~loc:e.pexpr_loc
                "cannot take address of non-lvalue expression";

            if t = Tnil then
              errorm ~loc:e.pexpr_loc "cannot take address of nil";

            (match te.expr_desc with
            | TEident v -> v.v_addr <- true
            | TEdot (base_expr, field) ->
                let rec mark_base expr =
                  match expr.expr_desc with
                  | TEident v -> v.v_addr <- true
                  | TEdot (b, f) -> mark_base b
                  | _ -> ()
                in

                mark_base base_expr
            | _ -> ());

            Tptr t
        | Ustar -> (
            match t with
            | Tptr t -> t
            | Tnil ->
                errorm ~loc:e.pexpr_loc "cannot dereference nil (type unknown)"
            | _ ->
                errorm ~loc:e.pexpr_loc "cannot dereference non-pointer type %s"
                  (string_of_typ t))
      in

      { expr_desc = TEunop (op, te); expr_typ = result_type }
  | PEnil -> { expr_desc = TEnil; expr_typ = Tnil }
  | PEcall (ident, pexpr_list) -> (
      if ident.id = "new" then begin
        if List.length pexpr_list <> 1 then
          errorm ~loc:ident.loc "new expects exactly 1 argument, got %d"
            (List.length pexpr_list);

        match (List.hd pexpr_list).pexpr_desc with
        | PEident type_ident -> (
            match Hashtbl.find_opt struct_env type_ident.id with
            | Some s ->
                { expr_desc = TEnew (Tstruct s); expr_typ = Tptr (Tstruct s) }
            | None ->
                errorm ~loc:type_ident.loc "undefined type: %s" type_ident.id)
        | _ -> errorm ~loc:ident.loc "new expects a type name as argument"
      end
      else
        match Hashtbl.find_opt func_env ident.id with
        | Some func_def ->
            if ident.id = "fmt.Print" then (
              let typed_args =
                List.map
                  (typecheck_expr struct_env func_env var_env
                     expected_return_type)
                  pexpr_list
              in
              fmt_print_used := true;
              { expr_desc = TEcall (func_def, typed_args); expr_typ = Tmany [] })
            else begin
              let typed_args =
                List.map
                  (typecheck_expr struct_env func_env var_env
                     expected_return_type)
                  pexpr_list
              in

              let arguments_types =
                ref (List.map (fun te -> te.expr_typ) typed_args)
              in

              (* check whether expression returns multiple values *)
              if
                List.length func_def.fn_params <> 1
                && List.length typed_args = 1
              then
                match (List.hd typed_args).expr_typ with
                | Tmany types when List.length types > 0 ->
                    arguments_types := types;
                    if List.length types <> List.length func_def.fn_params then
                      errorm ~loc:ident.loc
                        "function %s expects %d arguments, got %d" ident.id
                        (List.length func_def.fn_params)
                        (List.length types)
                | _ ->
                    errorm ~loc:ident.loc "function %s expects %d arguments"
                      ident.id
                      (List.length func_def.fn_params)
              else if List.length pexpr_list <> List.length func_def.fn_params
              then
                errorm ~loc:ident.loc "function %s expects %d arguments, got %d"
                  ident.id
                  (List.length func_def.fn_params)
                  (List.length pexpr_list);

              List.iter2
                (fun arg_type param_typ ->
                  if not (types_equal arg_type param_typ) then
                    errorm ~loc:ident.loc
                      "function %s argument type mismatch: expected %s, got %s"
                      ident.id (string_of_typ param_typ)
                      (string_of_typ arg_type))
                !arguments_types
                (List.map (fun v -> v.v_typ) func_def.fn_params);

              {
                expr_desc = TEcall (func_def, typed_args);
                expr_typ =
                  (match func_def.fn_typ with
                  | [] -> Tmany []
                  | [ t ] -> t
                  | ts -> Tmany ts);
              }
            end
        | None -> errorm ~loc:ident.loc "undefined function: %s" ident.id)
  | PEident ident -> (
      match find_opt_var_global var_env ident.id with
      | Some v ->
          if v.v_name = "_" then
            errorm ~loc:ident.loc "cannot use blank identifier '_' as variable";

          v.v_used <- true;
          { expr_desc = TEident v; expr_typ = v.v_typ }
      | None -> errorm ~loc:ident.loc "undefined variable: %s" ident.id)
  | PEdot (base_expr, field_ident) -> (
      let tbase_expr =
        typecheck_expr struct_env func_env var_env expected_return_type
          base_expr
      in

      match tbase_expr.expr_typ with
      | Tstruct s -> (
          match Hashtbl.find_opt s.s_fields field_ident.id with
          | Some field ->
              { expr_desc = TEdot (tbase_expr, field); expr_typ = field.f_typ }
          | None ->
              errorm ~loc:field_ident.loc "structure %s has no field named %s"
                s.s_name field_ident.id)
      (* special case for autodererencing *)
      | Tptr (Tstruct s) -> (
          match Hashtbl.find_opt s.s_fields field_ident.id with
          | Some field ->
              { expr_desc = TEdot (tbase_expr, field); expr_typ = field.f_typ }
          | None ->
              errorm ~loc:field_ident.loc "structure %s has no field named %s"
                s.s_name field_ident.id)
      | Tnil ->
          errorm ~loc:base_expr.pexpr_loc
            "cannot access field %s of nil (type unknown)" field_ident.id
      | _ ->
          errorm ~loc:base_expr.pexpr_loc
            "cannot access field %s of non-structure type %s" field_ident.id
            (string_of_typ tbase_expr.expr_typ))
  | PEassign (lhs_list, rhs_list) ->
      let t_lhs_list =
        List.map
          (typecheck_expr struct_env func_env var_env expected_return_type)
          lhs_list
      in
      let t_rhs_list =
        List.map
          (typecheck_expr struct_env func_env var_env expected_return_type)
          rhs_list
      in

      let rhs_types = ref (List.map (fun r -> r.expr_typ) t_rhs_list) in

      (* check lvalues *)
      List.iter
        (fun orig ->
          if not (is_lvalue orig) then
            errorm ~loc:orig.pexpr_loc
              "left-hand side of assignment must be an lvalue")
        lhs_list;

      (* check right side for Tmany *)
      if List.length lhs_list > 1 && List.length !rhs_types = 1 then begin
        match List.hd !rhs_types with
        | Tmany types ->
            rhs_types := types;

            if List.length lhs_list <> List.length types then
              errorm ~loc:e.pexpr_loc
                "assigment arity mismatch: %d lhs vs %d rhs"
                (List.length lhs_list) (List.length types)
        | _ ->
            errorm ~loc:e.pexpr_loc "assigment arity mismatch: %d lhs vs %d rhs"
              (List.length lhs_list) (List.length t_rhs_list)
      end;

      (* compare types of lhs and rhs *)
      List.iter2
        (fun left_type right_type ->
          if not (types_equal left_type right_type) then
            errorm ~loc:e.pexpr_loc
              "assignment type mismatch: expected %s, got %s"
              (string_of_typ left_type) (string_of_typ right_type))
        (List.map (fun l -> l.expr_typ) t_lhs_list)
        !rhs_types;

      { expr_desc = TEassign (t_lhs_list, t_rhs_list); expr_typ = Tmany [] }
  | PEvars (ident_list, opt_typ, init_exprs) ->
      let init_types =
        ref
          (List.map
             (fun expr ->
               (typecheck_expr struct_env func_env var_env expected_return_type
                  expr)
                 .expr_typ)
             init_exprs)
      in

      (* check if rhs is Tmany *)
      if List.length ident_list <> 1 && List.length !init_types = 1 then begin
        match List.hd !init_types with
        | Tmany types ->
            init_types := types;

            if List.length types <> List.length ident_list then
              errorm ~loc:e.pexpr_loc
                "variable declaration arity mismatch: expected %d, got %d"
                (List.length ident_list) (List.length types)
        | _ ->
            errorm ~loc:e.pexpr_loc "Var expects %d initialization arguments"
              (List.length ident_list)
      end;
      let deduced_types =
        match opt_typ with
        | Some ptyp ->
            let typ = ptyp_to_typ struct_env ptyp in

            (* check if init types are equal to the defined one *)
            (* TODO: Handle Scopes *)
            if
              List.length !init_types > 0
              && List.length !init_types <> List.length ident_list
            then
              errorm ~loc:e.pexpr_loc
                "variable declaration arity mismatch: expected %d, got %d"
                (List.length ident_list) (List.length !init_types);

            if List.length !init_types > 0 then
              List.iter
                (fun init_type ->
                  if not (types_equal typ init_type) then
                    errorm ~loc:e.pexpr_loc
                      "variable declaration type mismatch: expected %s, got %s"
                      (string_of_typ typ) (string_of_typ init_type))
                !init_types;
            List.map (fun _ -> typ) ident_list
        | None ->
            if List.length !init_types <> List.length ident_list then
              errorm ~loc:e.pexpr_loc
                "variable declaration arity mismatch: expected %d, got %d"
                (List.length ident_list) (List.length !init_types);

            (* without defined type we cannot have nil here *)
            List.iter
              (fun init_type ->
                if init_type = Tnil then
                  errorm ~loc:e.pexpr_loc
                    "cannot deduce type from nil initial value")
              !init_types;

            !init_types
      in

      (* declare variables with deduced types *)
      let created_variables =
        List.map2
          (fun ident deduced_type ->
            match find_opt_var_current_scope var_env ident.id with
            | Some vv ->
                if vv.v_name <> "_" then
                  errorm ~loc:ident.loc "variable %s already declared" ident.id;

                vv
            | None ->
                let v = new_var ident.id ident.loc deduced_type in
                add_var var_env v;
                v)
          ident_list deduced_types
      in

      { expr_desc = TEvars created_variables; expr_typ = Tmany [] }
  | PEif (cond, then_branch, else_branch) ->
      let cond_typed =
        typecheck_expr struct_env func_env var_env expected_return_type cond
      in
      if cond_typed.expr_typ <> Tbool then
        errorm ~loc:cond.pexpr_loc "if condition must be of type bool, got %s"
          (string_of_typ cond_typed.expr_typ);

      let then_typed =
        typecheck_expr struct_env func_env var_env expected_return_type
          then_branch
      in
      let else_types =
        typecheck_expr struct_env func_env var_env expected_return_type
          else_branch
      in

      {
        expr_desc = TEif (cond_typed, then_typed, else_types);
        expr_typ = Tmany [];
      }
  | PEreturn exprs ->
      let typed_exprs =
        List.map
          (typecheck_expr struct_env func_env var_env expected_return_type)
          exprs
      in

      let return_types = ref (List.map (fun te -> te.expr_typ) typed_exprs) in

      if List.length expected_return_type > 1 && List.length !return_types = 1
      then begin
        match List.hd !return_types with
        | Tmany types -> return_types := types
        | _ ->
            errorm ~loc:e.pexpr_loc "return arity mismatch: expected %d, got %d"
              (List.length expected_return_type)
              (List.length typed_exprs)
      end;

      (* check whether return type corresponds to the expected one *)
      if List.length !return_types <> List.length expected_return_type then
        errorm ~loc:e.pexpr_loc "return arity mismatch: expected %d, got %d"
          (List.length expected_return_type)
          (List.length !return_types);

      List.iter2
        (fun actual expected ->
          if not (types_equal actual expected) then
            errorm ~loc:e.pexpr_loc "return type mismatch: expected %s, got %s"
              (string_of_typ expected) (string_of_typ actual))
        !return_types expected_return_type;

      { expr_desc = TEreturn typed_exprs; expr_typ = Tmany [] }
  | PEblock exprs ->
      let env' = push_scope var_env in
      let typed_exprs =
        List.map
          (typecheck_expr struct_env func_env env' expected_return_type)
          exprs
      in
      { expr_desc = TEblock typed_exprs; expr_typ = Tmany [] }
  | PEfor (cond, block) ->
      let env' = push_scope var_env in
      let cond_typed =
        typecheck_expr struct_env func_env env' expected_return_type cond
      in
      if cond_typed.expr_typ <> Tbool then
        errorm ~loc:cond.pexpr_loc "for condition must be of type bool, got %s"
          (string_of_typ cond_typed.expr_typ);

      let block_typed =
        typecheck_expr struct_env func_env env' expected_return_type block
      in
      { expr_desc = TEfor (cond_typed, block_typed); expr_typ = Tmany [] }
  | PEincdec (expr, incdec) ->
      let t_expr =
        typecheck_expr struct_env func_env var_env expected_return_type expr
      in
      if t_expr.expr_typ <> Tint then
        errorm ~loc:expr.pexpr_loc
          "increment/decrement requires int type, got\n            %s"
          (string_of_typ t_expr.expr_typ);
      if not (is_lvalue expr) then
        errorm ~loc:expr.pexpr_loc "increment/decrement requires lvalue";
      { expr_desc = TEincdec (t_expr, incdec); expr_typ = Tint }

let typecheck_function (struct_env : struct_env) (func_env : func_env)
    (f : pfunc) : function_ * expr =
  let params = f.pf_params in

  (* check uniqueness of params*)
  check_duplicates
    ~get_key:(fun (ident, _typ) -> ident.id)
    ~on_duplicate:(fun (ident, _typ) ->
      begin
        errorm ~loc:ident.loc "duplicate parameter: %s for function: %s"
          ident.id f.pf_name.id
      end)
    params;

  (* create var env *)
  if not (Hashtbl.mem func_env f.pf_name.id) then
    errorm ~loc:f.pf_name.loc "undefined function: %s" f.pf_name.id;

  let func_def = Hashtbl.find func_env f.pf_name.id in

  let var_env = empty_env in
  List.iter (fun v -> add_var var_env v) func_def.fn_params;

  (* typecheck function body *)
  let typed_body =
    typecheck_expr struct_env func_env var_env func_def.fn_typ f.pf_body
  in

  if List.length func_def.fn_typ > 0 && not (always_returns typed_body) then
    errorm ~loc:f.pf_name.loc "Not all paths of function: %s return"
      func_def.fn_name;

  (* print function type *)
  if !debug then
    printf "function %s : (%s) -> (%s)\n" func_def.fn_name
      (String.concat ", "
         (List.map (fun v -> string_of_typ v.v_typ) func_def.fn_params))
      (String.concat ", " (List.map string_of_typ func_def.fn_typ));

  (func_def, typed_body)

let typecheck_structure (struct_env : struct_env) (s : pstruct) : structure =
  let fields = s.ps_fields in

  (* check uniqueness of fields *)
  check_duplicates
    ~get_key:(fun (ident, _typ) -> ident.id)
    ~on_duplicate:(fun (ident, _typ) ->
      errorm ~loc:ident.loc "duplicate field: %s for structure: %s" ident.id
        s.ps_name.id)
    fields;

  (* map fields and check them in the same time (check is done in the mapping func) *)
  let fields_list =
    List.map
      (fun (ident, ptyp) ->
        let _typ = ptyp_to_typ struct_env ptyp in
        { f_name = ident.id; f_typ = _typ; f_ofs = 0 })
      s.ps_fields
  in

  (* create hashtbl of fields *)
  let fields_hashtbl = Hashtbl.create (List.length fields_list) in
  List.iter
    (fun field -> Hashtbl.add fields_hashtbl field.f_name field)
    fields_list;

  {
    s_name = s.ps_name.id;
    s_fields = fields_hashtbl;
    s_list = fields_list;
    s_size = 0;
  }

let file ~debug:b ((imp, dl) : Ast.pfile) : Tast.tfile =
  debug := b;

  let list_of_structs =
    List.filter_map
      (fun decl -> match decl with PDstruct s -> Some s | _ -> None)
      dl
  in

  let list_of_functions =
    List.filter_map
      (fun decl -> match decl with PDfunction f -> Some f | _ -> None)
      dl
  in

  (* check function duplicates *)
  check_duplicates
    ~get_key:(fun (ident, _typ) -> ident.id)
    ~on_duplicate:(fun (ident, _typ) ->
      begin
        errorm ~loc:ident.loc "duplicate function: %s" ident.id
      end)
    (List.filter_map (fun f -> Some (f.pf_name, f.pf_typ)) list_of_functions);

  (* check struct duplicates *)
  check_duplicates
    ~get_key:(fun ident -> ident.id)
    ~on_duplicate:(fun ident ->
      begin
        errorm ~loc:ident.loc "duplicate structure: %s" ident.id
      end)
    (List.filter_map (fun s -> Some s.ps_name) list_of_structs);

  (* build struct env *)
  let struct_env = Hashtbl.create (List.length list_of_structs) in

  (* add empty records with structures at the beginning *)
  List.iter
    (fun s ->
      let structure =
        {
          s_name = s.ps_name.id;
          s_fields = Hashtbl.create 10;
          s_list = [];
          s_size = 0;
        }
      in
      Hashtbl.add struct_env s.ps_name.id structure)
    list_of_structs;

  (* Now populate the fields in-place *)
  List.iter
    (fun s ->
      let structure = Hashtbl.find struct_env s.ps_name.id in
      (* Build fields and populate the existing structure *)
      List.iter
        (fun (fname, ftyp) ->
          let field =
            {
              f_name = fname.id;
              f_typ = ptyp_to_typ struct_env ftyp;
              f_ofs = 0;
            }
          in
          Hashtbl.add structure.s_fields fname.id field;
          structure.s_list <- field :: structure.s_list)
        s.ps_fields;
      structure.s_list <- List.rev structure.s_list)
    list_of_structs;

  (* check struct's cycles *)
  let dep_graph = build_graph list_of_structs in

  let cycles = detect_cycles list_of_structs dep_graph in

  if List.length cycles > 0 then
    errorm "Found following cycles in the structures declarations:\n %s "
      (String.concat "\n" cycles);

  (* print known structures *)
  Hashtbl.iter
    (fun name struct_def -> Printf.printf "known structure: %s\n" name)
    struct_env;

  (* build func env *)
  let func_env = Hashtbl.create (List.length list_of_functions) in
  List.iter
    (fun f ->
      let func_def = build_function struct_env f in
      Hashtbl.add func_env f.pf_name.id func_def)
    list_of_functions;

  (* add builtin functions *)
  if imp then begin
    let builtin_print =
      { fn_name = "fmt.Print"; fn_params = []; fn_typ = [] }
    in
    Hashtbl.add func_env builtin_print.fn_name builtin_print
  end;

  let typed_decls =
    List.map
      (fun decl ->
        match decl with
        | PDstruct s -> TDstruct (typecheck_structure struct_env s)
        | PDfunction f ->
            let func_def, typed_body =
              typecheck_function struct_env func_env f
            in

            let vars_inside_func =
              collect_declared_vars typed_body @ func_def.fn_params
            in

            List.iter
              (fun v ->
                if not v.v_used then
                  errorm ~loc:v.v_loc "variable %s declared but not used"
                    v.v_name)
              (List.filter (fun v -> v.v_name <> "_") vars_inside_func);

            TDfunction (func_def, typed_body))
      dl
  in

  (* check whether import for fmt is used *)
  if imp && not !fmt_print_used then
    errorm ~loc:dummy_loc "imported package fmt is not used";

  (* check whether there is function main without arguments and without return type *)
  (match Hashtbl.find_opt func_env "main" with
  | Some main_func ->
      if List.length main_func.fn_params <> 0 then
        errorm ~loc:dummy_loc "function main must not have parameters, got %d"
          (List.length main_func.fn_params);
      if List.length main_func.fn_typ <> 0 then
        errorm ~loc:dummy_loc
          "function main must not have return values, got %d"
          (List.length main_func.fn_typ)
  | None -> errorm ~loc:dummy_loc "function main is not defined");

  typed_decls
