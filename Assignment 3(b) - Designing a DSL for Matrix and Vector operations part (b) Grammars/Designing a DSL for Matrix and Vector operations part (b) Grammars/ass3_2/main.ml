(* main.ml *)
open Lexer
open Parser
open Lexing
open Ast
open Parsing
open Type_checker

let parse_input lexbuf =
  try
    let ast = Parser.program Lexer.token lexbuf in
    ast
  with
  | Lexer.Lexing_error msg ->
      Printf.eprintf "Lexing error: %s\n" msg;
      exit 1
  | Parse_error ->
      let pos = lexeme_start_p lexbuf in
      Printf.eprintf "Parse error at line %d, column %d\n"
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
      exit 1

let () =
  print_endline "Enter your DSL code (press Ctrl-D to finish):";
  let lexbuf = Lexing.from_channel stdin in
  let ast = parse_input lexbuf in
  Printf.printf "Parsed AST:\n%s\n" (string_of_ast ast);
  
  (* Type checking *)
  let success, msg, _ = typecheck_program ast in
  if success then (
    Printf.printf "\nType checking successful✅🥳\n";
    print_env ()
  ) else
    Printf.printf "\n%s\n" msg
