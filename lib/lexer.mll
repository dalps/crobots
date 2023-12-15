{
  open Parser

  exception Error of string
}

let white = [' ' '\t']+

let letter = ['a'-'z''A'-'Z']
let digit = ['0'-'9']
let id = ['a'-'z'] (['_'] | letter | digit)*
let nat = digit+
let big_comment = "/*" [^'*''/']* "*/"
let line_comment = "//" [^'\n']*

rule read_token = parse
| '\n' { Lexing.new_line lexbuf; read_token lexbuf }
| white | big_comment | line_comment { read_token lexbuf }
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
| "&&" { LAND }
| "||" { LOR }
| '<' { LT }
| '>' { GT }
| "<=" { LEQ }
| ">=" { GEQ }
| "==" { EQ }
| "!=" { NEQ }
| "++" { INCR }
| "--" { DECR }
| "int" { INT_TYPE }
| "return" { RETURN }
| "if" { IF }
| "else" { ELSE }
| "while" { WHILE }
| "do" { DO }
| id { IDE (Lexing.lexeme lexbuf) }
| nat { CONST (Lexing.lexeme lexbuf |> int_of_string) }
| _ { raise (Error "unexpected character") }
| eof { EOF }