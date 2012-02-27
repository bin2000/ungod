{
  open Printf
  open Lexing
  open Type
  open Parser
  open Util.Lexer
}

let ident = ['a'-'z' 'A'-'Z']['_' '0'-'9' 'a'-'z' 'A'-'Z']*
let dg = ['0'-'9']
let int_exp =  '0' | ['1' - '9'] dg * 
let float_exp = int_exp ('.' (dg*)? 'f'? | 'f')
let bits = ['0' '1'] + 'b'
let char_exp = '"' ([^'"'] | "\\\"") '"'
let escaped = ['\\' 'n' 't' 's' 'r' '"']
let str_exp = ([^'"' '\\'] | ('\\' escaped)) *
let symbol = '`' ident? (* this is wrong. actually you can do something like `100*)
let type_exp = int_exp 'h' (* yea apparent this is not the same as an integer *)
let date_exp = ['1'-'9'] dg dg dg '.' dg dg '.' dg dg
let time_exp = dg dg ':' dg dg ':' dg dg ('.' (dg dg dg) )?
(*let inline_comment = '/' [^'\n']+  [^' ' '\t' '\n'] [^'\n']*  *)


rule prog = parse
  | "\n/" [' ' '\t']* '\n'
    {update_loc lexbuf 2 0; comment lexbuf}
  | '/' [' ' '\t']* '\n'
    {update_loc lexbuf 1 0; comment lexbuf}
  | '\n' ? '/' as t
    {
      if t.[0] = '\n' then begin
        State.set_commentable true;
        update_loc lexbuf 1 0 end
      else ();
      update_loc lexbuf 0 1;
      if State.is_commentable () then inline_comment lexbuf
      else BUILTIN "/"
   }
  | '\\'
    {if lexbuf.lex_curr_p.pos_bol = 0
      then syscmd lexbuf
      else BUILTIN "\\"}
  | '\n'
    {
      State.set_commentable true;
      update_loc lexbuf 1 0; NEWLINE}
  | '\n' + [' ' '\t' ] as t
    {State.set_commentable true;
      update_loc lexbuf ((String.length t) - 1) 1; prog lexbuf}
  | [' ' '\t']
    {
      State.set_commentable true;
      update_loc lexbuf 0 1; prog lexbuf}
  | ("::" | "/:" | ":'" | "\\:" | "`:" | "+:" | "-:" | "*:" | "%:" | ",:" |
     "til" | "asc" | "desc" ) as t
    {State.set_commentable false; update_loc lexbuf 0 (String.length t); BUILTIN t}
  | ['+' '-' '*' '\'' '%' ',' '.' '?' '_' '!' '@' '#' 
     '$' '^' '&' '|' '<' '>' '~' '`' '=' ':'] as t
    {State.set_commentable false; update_loc lexbuf 0 1; BUILTIN (String.make 1 t)}
  | '"' {update_loc lexbuf 0 1; string lexbuf}
  | "select" 
    {update_loc lexbuf 0 6; SELECT}
  | "delete" 
    {update_loc lexbuf 0 6; DELETE}
  | "update" 
    {update_loc lexbuf 0 6; UPDATE} 
  | "exec" {update_loc lexbuf 0 4; EXEC}
  | "by" {update_loc lexbuf 0 2; BY}
  | "from" {update_loc lexbuf 0 4; FROM}
  | date_exp "T" time_exp as t 
    {update_loc lexbuf 0 (String.length t); DATETIME (to_datetime t)}  
  | time_exp as t
    {update_loc lexbuf 0 (String.length t); TIMESTAMP (to_timestamp t)}
  | date_exp as t
    {update_loc lexbuf 0 (String.length t); DATE (to_date t)}
  | char_exp as t
    {update_loc lexbuf 0 (String.length t); CHAR t.[1]}
  | float_exp as t
    {update_loc lexbuf 0 (String.length t); FLOAT (float_of_string (trim_f t))}
  | type_exp as t
    {update_loc lexbuf 0 (String.length t); QTYPE (qtype_of_string t)}
  | bits as t
    {update_loc lexbuf 0 (String.length t); BITS (to_bits t)}
  | int_exp as t 
    {update_loc lexbuf 0 (String.length t); INT (int_of_string t)}
  | symbol + as t
    {update_loc lexbuf 0 (String.length t); SYMLIST (to_symlist t)}
  | ident as t
    {
      State.set_commentable false;
      update_loc lexbuf 0 (String.length t);
      IDENT {id_name = t; id_value = None}
    }
  | ';'
    {State.set_commentable true; update_loc lexbuf 0 1; SEMICOLON}
  | '('
    {State.ParenCt.incr (); update_loc lexbuf 0 1; PAREN_LEFT}
  | ')'
    {
      State.set_commentable false;
      State.ParenCt.decr lexbuf.lex_curr_p;
      update_loc lexbuf 0 1;
      PAREN_RIGHT
    }
  | '['
    {State.BracketCt.incr (); update_loc lexbuf 0 1; BRACKET_LEFT}
  | ']'
    {
      State.set_commentable false;
      State.BracketCt.decr lexbuf.lex_curr_p;
      update_loc lexbuf 0 1;
      BRACKET_RIGHT
    }
  | '{'
    {State.BraceCt.incr (); update_loc lexbuf 0 1; CURLY_LEFT}
  | '}'
    {
      State.set_commentable false;
      State.BraceCt.decr lexbuf.lex_curr_p;
      update_loc lexbuf 0 1;
      CURLY_RIGHT
    }
  | eof
    {State.check_parity (); EOF}
 
and string = parse
  |  str_exp eof
    {State.notify_error "Unterminated string!"} 
  |  str_exp '"' as t
    {update_loc lexbuf 0 (String.length t); STR (to_charlist t)}
  
and inline_comment = parse
  [^'\n']* [^' ' '\t' '\n'] [^'\n']* {
    assert (State.is_commentable ());
    State.set_commentable false;
    prog lexbuf}

and comment = parse
  | '\\' [' ' '\t']* '\n' {update_loc lexbuf 1 0; prog lexbuf}
  | '\\' [' ' '\t']*  eof  {State.check_parity (); EOF}
  | '\n' {update_loc lexbuf 1 0; comment lexbuf}
  | [^'\\' '\n'] {update_loc lexbuf 0 1; comment lexbuf}
  | eof {State.check_parity (); EOF}

and syscmd = parse
  [^'\t' ' ' '\n'] [^'\n'] * '\n'
  {
    update_loc lexbuf 1 0;
    prog lexbuf}
  
