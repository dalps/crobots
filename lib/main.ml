let parse' parser text =
  let lexbuf = Lexing.from_string text in
  try parser Lexer.read_token lexbuf
  with exn ->
    let pos = lexbuf.lex_curr_p
    and errstr =
      match exn with
      | Lexer.Error msg -> msg
      | Parser.Error -> "syntax error"
      | _ -> "weird error"
    in
    failwith
      (Printf.sprintf "line %d, column %d: %s%!" pos.pos_lnum
         (pos.pos_cnum - pos.pos_bol)
         errstr)

let parse = parse' Parser.main
