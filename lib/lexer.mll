{
  open Parser
}

let white = [' ' '\t']+

let digit = ['0'-'9']
let int_const = digit+

rule read_token = parse
| white { read_token lexbuf }
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
| int_const { INT_CONST (Lexing.lexeme lexbuf) }
| eof { EOL }