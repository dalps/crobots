{
  open Parser

  exception Error of string
}

let white = [' ' '\t']+

let letter = ['a'-'z''A'-'Z']
let digit = ['0'-'9']
let id = ['a'-'z'] (['_'] | letter | digit)*
let int_const = digit+

rule read_token = parse
| white { read_token lexbuf }
| '\n' { Lexing.new_line lexbuf; read_token lexbuf }
| "if" { IF }
| "else" { ELSE }
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LBRACE }
| '}' { RBRACE }
| ';' { SEMICOLON }
| '*' { MUL }
| '/' { DIV }
| '%' { MOD }
| '+' { ADD }
| '-' { MINUS }
| '=' { ASSIGN }
| ',' { COMMA }
| "int" { INT_TYPE }
| id { ID (Lexing.lexeme lexbuf) }
| int_const { INT_CONST (Lexing.lexeme lexbuf) }
| _ { raise (Error "unexpected character") }
| eof { EOF }