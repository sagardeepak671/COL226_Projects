open Lexing
open Lexer

let rec print_tokens lexbuf =
  let tok = token lexbuf in
  match tok with
  | END_OF_FILE -> print_endline "EOF"
  | _ ->
      Printf.printf "%s\n" (
        match tok with
        |INPUT -> "INPUT"
        |PRINT -> "PRINT"
        |ASSIGN -> "ASSIGN"
        |PLUS -> "PLUS"
        |MINUS -> "MINUS"
        |DIVIDE -> "DIVIDE"
        |MULTIPLY -> "MULTIPLY"
        |EQ -> "EQ"
        |NEQ -> "NEQ"
        |LE -> "LE"
        |LT -> "LT"
        |GE -> "GE"
        |GT -> "GT"
        |AND -> "AND"
        |OR -> "OR"
        |NOT -> "NOT"
        |RPAREN -> "RPAREN"
        |LPAREN -> "LPAREN"
        |RBRACKET -> "RBRACKET"
        |LBRACKET -> "LBRACKET"
        |RBRACE -> "RBRACE"
        |LBRACE -> "LBRACE"
        |SEMICOLON -> "SEMICOLON"
        |COMMA -> "COMMA"
        |IF -> "IF"
        |THEN -> "THEN"
        |ELSE -> "ELSE"
        |FOR -> "FOR"
        |WHILE -> "WHILE"
        |ABS -> "ABS"

        |ADD_VECTOR -> "ADD_VECTOR"
        |SCALE_VECTOR -> "SCALE_VECTOR"
        |DOT_VECTOR -> "DOT_VECTOR"
        |ANGLE -> "ANGLE"
        |MAGNITUDE -> "MAGNITUDE"
        |DIMENSION -> "DIMENSION"

        |ADD_MATRIX -> "ADD_MATRIX"
        |SCALE_MATRIX -> "SCALE_MATRIX"
        |MULTIPLY_MATRIX -> "MULTIPLY_MATRIX"
        |TRANSPOSE -> "TRANSPOSE"
        |DETERMINANT -> "DETERMINANT"

        |TYPE_BOOL -> "TYPE_BOOL"
        |TYPE_INT -> "TYPE_INT"
        |TYPE_FLOAT -> "TYPE_FLOAT"
        |TYPE_VECTOR -> "TYPE_VECTOR"
        |TYPE_MATRIX -> "TYPE_MATRIX"
        |TRUE_VAL -> "TRUE_VAL"
        |FALSE_VAL -> "FALSE_VAL"

        |STRING_LITERAL s -> Printf.sprintf "STRING_LITERAL(%s)" s

        |IDENTIFIER id -> Printf.sprintf "IDENTIFIER(%s)" id
        |FLOAT_VAL f -> Printf.sprintf "FLOAT_VAL(%f)" f
        |INT_VAL n -> Printf.sprintf "INT_VAL(%d)" n
        |END_OF_FILE -> "END_OF_FILE" 
      );
      print_tokens lexbuf

let () =
  let lexbuf = Lexing.from_channel stdin in
  print_tokens lexbuf