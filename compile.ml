(** Code generation for Mini Go programs (TODO) *)

open Format
open Ast
open Tast
open X86_64
open Compilation

let debug = ref false
let iter f = List.fold_left (fun code x -> code ++ f x) nop
let iter2 f = List.fold_left2 (fun code x y -> code ++ f x y) nop

let file ?debug:(b = false) (dl : Tast.tfile) : X86_64.program =
  debug := b;

  (* compile functions *)
  let funcs =
    List.fold_left
      (fun code -> function
        | TDfunction (func, body) ->
            let code_f = Compilation.compile_function func body in
            code ++ code_f
        | _ -> assert false)
      nop dl
  in

  {
    text =
      globl "main" ++ funcs
      ++ inline "\n# TODO some auxiliary assembly functions, if needed\n"
      ++ aligned_call_wrapper ~f:"malloc" ~newf:"malloc_"
      ++ aligned_call_wrapper ~f:"calloc" ~newf:"calloc_"
      ++ aligned_call_wrapper ~f:"printf" ~newf:"printf_";
    data = Data.generate_data_section dl;
  }
