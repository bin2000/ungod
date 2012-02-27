module S = Semantics
;;

let from_file filepath =
  let inc = open_in filepath in
  Util.Lexer.State.reset ();
  let lexbuf = Lexing.from_channel inc in
  lexbuf.Lexing.lex_curr_p <- {
    Lexing.pos_fname = filepath;
    Lexing.pos_lnum = 1;
    Lexing.pos_bol = 0;
    Lexing.pos_cnum = 0 };
  Parser.prog Lexer.prog lexbuf
;;

let from_string s  =
  Util.Lexer.State.reset ();
  Parser.prog Lexer.prog (Lexing.from_string s);
;;
