{
  open Parser

  exception Error of string
}

let white = [' ' '\t']+

let letter = ['a'-'z''A'-'Z']
let digit = ['0'-'9']
let id = ['a'-'z'] (['_'] | letter | digit)*
let nat = digit+
let line_comment = "//" [^'\n']*

rule read_token = parse
| '\n' { Lexing.new_line lexbuf; read_token lexbuf }
| white | line_comment { read_token lexbuf }
| "/*" { multiline_comment lexbuf }
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
| _ { raise (Error (Lexing.lexeme lexbuf)) }
| eof { EOF }

(* Multi-line comment terminated by "*/"
   From https://github.com/jhjourdan/C11parser/blob/master/lexer.mll *)
and multiline_comment = parse
| "*/" { read_token lexbuf }
| eof { failwith "unterminated comment" }
| '\n' { Lexing.new_line lexbuf; multiline_comment lexbuf }
| _ { multiline_comment lexbuf }
