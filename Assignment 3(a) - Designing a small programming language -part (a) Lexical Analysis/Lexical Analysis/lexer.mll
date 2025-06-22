{ 
type token = 
    |INPUT|PRINT
    |ASSIGN|PLUS|MINUS|MULTIPLY|DIVIDE|ABS
    |IF|THEN|ELSE|FOR|WHILE 
    |TYPE_BOOL|TYPE_INT|TYPE_FLOAT|TYPE_VECTOR|TYPE_MATRIX
    |EQ|NEQ|GT|LT|GE|LE 
    |AND|OR|NOT 
    |ADD_VECTOR|SCALE_VECTOR|DOT_VECTOR|ANGLE|MAGNITUDE|DIMENSION
    |ADD_MATRIX|SCALE_MATRIX|MULTIPLY_MATRIX|TRANSPOSE|DETERMINANT
    |LPAREN|RPAREN|LBRACKET|RBRACKET|LBRACE|RBRACE|COMMA|SEMICOLON
    |STRING_LITERAL of string
    |IDENTIFIER of string
    |FLOAT_VAL of float
    |INT_VAL of int
    |TRUE_VAL|FALSE_VAL
    |END_OF_FILE
}

rule token = parse 
  | [' ' '\t' '\n']+             { token lexbuf }
  | "/*"                         { comment lexbuf }
  | "//"                         { single_comment lexbuf }
 
  | "Input"                      {INPUT}
  | "Print"                      {PRINT}
  | ":="                         {ASSIGN}
  | "+"                          {PLUS}
  | "-"                          {MINUS}
  | "/"                          {DIVIDE}
  | "*"                          {MULTIPLY}
  | "=="                         {EQ}
  | "!="                         {NEQ}
  | "<="                         {LE}
  | "<"                          {LT}
  | ">="                         {GE}
  | ">"                          {GT}
  | "&&"                         {AND}
  | "||"                         {OR}
  | "!"                          {NOT}
  | ")"                          {RPAREN}
  | "("                          {LPAREN}
  | "]"                          {RBRACKET}
  | "["                          {LBRACKET}
  | "}"                          {RBRACE}
  | "{"                          {LBRACE}
  | ";"                          {SEMICOLON}
  | ","                          {COMMA}
  | "if"                         {IF}
  | "then"                       {THEN}
  | "else"                       {ELSE}
  | "for"                        {FOR}
  | "while"                      {WHILE}
  | "abs"                        {ABS}  

  | "addv"                       {ADD_VECTOR}
  | "scalev"                     {SCALE_VECTOR}
  | "dotv"                       {DOT_VECTOR}
  | "angle"                      {ANGLE}
  | "magnitude"                  {MAGNITUDE}
  | "dimension"                  {DIMENSION}

  | "addm"                       {ADD_MATRIX}  
  | "scalem"                     {SCALE_MATRIX}
  | "multm"                      {MULTIPLY_MATRIX}
  | "transpose"                  {TRANSPOSE}
  | "determinant"                {DETERMINANT}


 
  | "bool"                       {TYPE_BOOL}
  | "int"                        {TYPE_INT}
  | "float"                      {TYPE_FLOAT}
  | "vector"                     {TYPE_VECTOR}
  | "matrix"                     {TYPE_MATRIX}
  | "true"                       {TRUE_VAL}
  | "false"                      {FALSE_VAL}  
 
  | '"' ([^ '"'])* '"' as string_literal      {let len = String.length string_literal in STRING_LITERAL (String.sub string_literal 1 (len - 2))} 
  
  | ['0'-'9']+ '.' ['0'-'9']* (['e' 'E'] ['+' '-']? ['0'-'9']+)? as float_num  {FLOAT_VAL (float_of_string float_num) }
  | '.' ['0'-'9']+ (['e' 'E'] ['+' '-']? ['0'-'9']+)? as float_num  {FLOAT_VAL (float_of_string float_num) }
  | ['0'-'9']+ ['e' 'E'] ['+' '-']? ['0'-'9']+ as float_num  {FLOAT_VAL (float_of_string float_num) }

  | ['0'-'9']+ as int_num                  {INT_VAL (int_of_string int_num) } 
  | ['a'-'z' 'A'-'Z' '_' '\''] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']* as identifier       {IDENTIFIER identifier} 
  | eof                              {END_OF_FILE}

and comment = parse
  | "*/"                        { token lexbuf }
  | _                           { comment lexbuf }

and single_comment = parse
  | '\n'                        { token lexbuf }
  | _                           { single_comment lexbuf }