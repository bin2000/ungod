type 'a ast_node = {
  node:'a;
  pos:Lexing.position
}

and q_atom =
  | Int of int
  | Float of float
  | Char of char
  | Qtype of int
  | Date of q_date
  | Timestamp of timestamp
  | Datetime of (q_date * timestamp)
  | Sym of string

and q_date = {
  year:int;
  month:int;
  date:int;
}

and timestamp = {
  hour:int;
  min:int;
  sec:int;
  milli:int;
}

and lambda = {
  lambda_args:expr list;
  lambda_body:expr list;
}
  
and table = {
  key_defs:expr list;
  columns:expr list;
}

and ident = {
  id_name:string;
  mutable id_value: expr option;
}

and modified = {
  orig:expr;
  modifier:string;
}


and expr = 
  | Ident of ident ast_node
  | Apply of apply
  | Builtin of string ast_node
  | Atom of q_atom ast_node
  | Protected of expr
  | ModifiedDyadic of modified
  | List of expr list ast_node
  | Block of expr list ast_node
  | Lambda of lambda ast_node
  | Sql of sql_stmt
  | Table of table
  | Empty

and apply = {
  fun_id:expr;
  arg:expr; (* None corresponds to calling with no args *)
}

and sql_stmt = {
  header:sql_header ast_node;
  selector:expr list;
  by:expr list;
  table:expr;
  where:expr list;
}

and sql_header =
  | Select
  | Exec
  | Update
  | Delete

(* those are types for semantic checking *)  
type typed_node = 
  | QtypeT
  | NumT
  | CharT
  | SymT
  | DateT
  | TimeT
  | DatetimeT
  | TableT
  | ListT of typed_node list
  | UListT of typed_node (* U stands for Uniform *)
  | FunT of (typed_node list) list
  | WildcardT
  | EmptyT
