%{
  open Util.Parser
  open Type
%}

%start prog
%token EOF
%token CURLY_LEFT
%token CURLY_RIGHT
%token BRACKET_LEFT
%token BRACKET_RIGHT
%token SEMICOLON
%token NEWLINE
%token SLASH
%token BACKSLASH
%token <string list> SYMLIST
%token <string> BUILTIN
%token <int> INT
%token <Type.q_date> DATE
%token <Type.timestamp> TIMESTAMP
%token <(Type.q_date * Type.timestamp)> DATETIME
%token <int> QTYPE
%token <float> FLOAT
%token <char> CHAR
%token <char list> STR
%token <int list> BITS
%token <Type.ident> IDENT
%token PAREN_RIGHT
%token PAREN_LEFT
%token SELECT
%token UPDATE
%token FROM
%token BY
%token DELETE
%token EXEC

%type <Type.expr list> prog

%%
prog:
  | EOF {[]}
  | expr_list EOF {$1}
  | delimiters expr_list EOF {$2}
  | delimiters {[]}
  
expr_list:
  | post_expr delimiters expr_list {$1 :: $3}
  | post_expr {[$1]}
  | post_expr delimiters {[$1]}

delimiters:
  | SEMICOLON delimiters {}
  | NEWLINE delimiters {}
  | SEMICOLON {}
  | NEWLINE {}

post_expr: expr {$1 ==> make_intlist
                     ==> make_floatlist
                     ==> make_datelist
                     ==> make_timestamplist
                     ==> make_datetimelist
                     ==> apply_adverb
                     ==> make_dyadic}

expr:
  | callable {$1}
  | application {$1}
  | sql_stmt {Sql $1} 

unsql_expr:
  | callable {$1}
  | application {$1 }

atom:
  | QTYPE {Qtype $1}
  | INT {Int $1}
  | FLOAT {Float $1}
  | DATE {Date $1}
  | TIMESTAMP {Timestamp $1}
  | DATETIME {Datetime $1}
  | CHAR {Char $1}

qlist:
  | PAREN_LEFT PAREN_RIGHT {{node = []; pos = rhs_start_pos 1}}
  | PAREN_LEFT list_inside PAREN_RIGHT {
      let p = rhs_start_pos 1 in
      {node = $2; pos = p}}
  | STR {
      let p = rhs_start_pos 1 in 
      let n = List.map (fun x -> Atom {node = Char x; pos = p}) $1 in
      {node = n; pos = p}}
  | SYMLIST {
      let p = rhs_start_pos 1 in
      let n = List.map (fun x -> Atom {node = Sym x; pos = p}) $1 in
      {node = n; pos = p}}
  | BITS {
      let p = rhs_start_pos 1 in
      let n = List.map (fun x -> Atom {node = Int x; pos = p}) $1 in
      {node = n; pos = p}}

list_inside:
  expr SEMICOLON stmts {$1 :: $3}
  
application:
  | callable expr {Apply {fun_id = $1; arg = $2;}}
  
callable:
  | IDENT {Ident {node = $1; pos = rhs_start_pos 1}}
  | PAREN_LEFT expr PAREN_RIGHT {identify_table (Protected $2)}
  | args {Block $1}
  | qlist {List $1}
  | atom {Atom {node = $1; pos = rhs_start_pos 1}}
  | lambda {Lambda $1}
  | BUILTIN {Builtin {node = $1; pos = rhs_start_pos 1}}

lambda:
  | CURLY_LEFT stmts CURLY_RIGHT
    {let p = rhs_start_pos 1 in
      {node = identify_lambda_args {lambda_args = []; lambda_body = $2} p ; pos = p}}
  | CURLY_LEFT CURLY_RIGHT 
    {let p = rhs_start_pos 1 in
      {node = identify_lambda_args {lambda_args = []; lambda_body = []} p; pos = p}}

stmts:
  | expr {[$1]}
  | SEMICOLON {[Empty; Empty]}
  | expr SEMICOLON {[$1; Empty]}
  | expr SEMICOLON stmts {$1 :: $3}
  | SEMICOLON stmts {Empty :: $2}

args:
  | BRACKET_LEFT BRACKET_RIGHT {{node = []; pos = rhs_start_pos 1}}
  | BRACKET_LEFT stmts BRACKET_RIGHT {{node = $2; pos = rhs_start_pos 1}}

sql_stmt:
  | sql_upto_from FROM sql_after_from
    {
      let h, s, b = $1 in
      let t, w = $3 in {
        header = h;
        selector = s;
        by = b;
        table = t;
        where = w;
      }
    }  

sql_upto_from:
  | sql_header {($1, [], [])}
  | sql_header sql_selector_by {let s, b = $2 in ($1, s, b)}

sql_selector_by:
  | sql_selector {($1, [])}
  | sql_selector BY sql_selector {($1, $3)}

sql_after_from:
  | unsql_expr {extract_where_clause $1}
  | sql_stmt {(Sql $1, [])}


sql_header:
  | SELECT {{node = Select; pos = rhs_start_pos 1}}
  | UPDATE {{node = Update; pos = rhs_start_pos 1}}
  | DELETE {{node = Delete; pos = rhs_start_pos 1}}
  | EXEC {{node = Exec; pos = rhs_start_pos 1}}

sql_selector:
  unsql_expr {make_sql_clauses $1}
