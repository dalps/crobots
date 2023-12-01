open Ast

let parse line : expr =
  let linebuf = Lexing.from_string line in
  Parser.main Lexer.read_token linebuf