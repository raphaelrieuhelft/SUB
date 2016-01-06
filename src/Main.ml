(* The driver: parsing, type-checking, elaboration, optimization,
   compilation, execution.

   Can be used both in interactive toplevel mode (if no file name
   given) or in batch mode (if a file name is given, containing
   zero, one or several expressions, separated by ';;').
*)

open Printf
open AST

(* Lexing and parsing *)

let print_location oc lb =
  fprintf oc "%s:%d:%d"
    Lexing.(lb.lex_curr_p.pos_fname)
    Lexing.(lb.lex_curr_p.pos_lnum)
    Lexing.(lb.lex_curr_p.pos_cnum - lb.lex_curr_p.pos_bol + 1)

let parse entrypoint lb =
  try
    Some(entrypoint Lexer.token lb)
  with
  | Lexer.Error msg ->
      eprintf "%a: %s\n" print_location lb msg; None
  | Parsing.Parse_error ->
      eprintf "%a: Syntax error\n" print_location lb; None
  | AST.Duplicate_label lbl ->
      eprintf "%a: Repeated label '%s'\n" print_location lb lbl; None

(* Given a SUB expression [e], performs type-checking, elaboration,
   optimization, compilation, and execution. *)

let process_expr e =
  try
    printf "-- Typing:\n";
    let t = Typing.infer empty_typenv e in
    printf "Well-typed, principal type: %s.\n" (pretty_typ t);
    let (t', l) = Elab.elab empty_typenv e in
    printf "-- Elaboration: type %s, term is below.\n" (pretty_typ t);
    IL.print_lam l;
    let l' = Optimize.optim l in
    printf "-- Optimization:\n";
    IL.print_lam l';
    let l'' = Expand.expand l' in
    printf "-- Expansion:\n";
    IL.print_lam l'';
    let c = Compile.program l'' in
    printf "-- Machine code:\n";
    Mach.print_program c;
    printf "-- Execution of the machine code:\n";
    Mach.execute_program c t
  with
  | Typing.Type_error msg ->
      printf "Ill-typed: %s\n" msg
  | Elab.Type_error msg ->
      printf "Ill-typed (during elaboration): %s\n" msg
  | Compile.Error msg ->
      printf "Compilation error: %s\n" msg

(* Process a file containing zero, one or several expressions. *)

let process_file filename =
  let ic = open_in filename in
  let lb = Lexing.from_channel ic in
  Lexing.(lb.lex_curr_p <- { pos_fname = filename; pos_lnum = 1;
                             pos_bol = 0; pos_cnum = 0 });
  match parse Parser.file lb with
  | None -> close_in ic; exit 2
  | Some el ->
      List.iteri
        (fun i e ->
          printf "--------- Phrase #%d ----------\n" (i + 1);
          process_expr e)
        el

(* Interactive toplevel loop. *)

let process_interactive () =
  let lb = Lexing.from_channel stdin in
  Lexing.(lb.lex_curr_p <- { pos_fname = "Toplevel"; pos_lnum = 1;
                             pos_bol = 0; pos_cnum = 0 });
  let rec toploop () =
    printf "\n# %!";
    match parse Parser.phrase lb with
    | None -> flush stderr; resynchronize (); toploop ()
    | Some None -> exit 0
    | Some (Some e) ->
        process_expr e;
        flush stdout; flush stderr;
        toploop()
  and resynchronize () =
    if Parsing.is_current_lookahead Parser.SEMISEMI
    || Parsing.is_current_lookahead Parser.EOF
    then ()
    else skip_phrase ()
  and skip_phrase () =
    try
      match Lexer.token lb with
      | Parser.SEMISEMI | Parser.EOF -> ()
      | _ -> skip_phrase ()
    with Lexer.Error _ -> skip_phrase ()
  in toploop ()

(* Main entry point *)

let _ =
  Printexc.record_backtrace true;
  if Array.length Sys.argv > 1 then
    for i = 1 to Array.length Sys.argv - 1 do process_file Sys.argv.(i) done
  else
    process_interactive ()

