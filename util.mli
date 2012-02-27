module Std :
  sig
    module Hashtbl :
      sig
        type ('a, 'b) t = ('a, 'b) Hashtbl.t
        val create : int -> ('a, 'b) t
        val clear : ('a, 'b) t -> unit
        val add : ('a, 'b) t -> 'a -> 'b -> unit
        val copy : ('a, 'b) t -> ('a, 'b) t
        val find : ('a, 'b) t -> 'a -> 'b
        val find_all : ('a, 'b) t -> 'a -> 'b list
        val mem : ('a, 'b) t -> 'a -> bool
        val remove : ('a, 'b) t -> 'a -> unit
        val replace : ('a, 'b) t -> 'a -> 'b -> unit
        val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
        val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
        val length : ('a, 'b) t -> int
        module type HashedType =
          sig type t val equal : t -> t -> bool val hash : t -> int end
        module type S =
          sig
            type key
            type 'a t
            val create : int -> 'a t
            val clear : 'a t -> unit
            val copy : 'a t -> 'a t
            val add : 'a t -> key -> 'a -> unit
            val remove : 'a t -> key -> unit
            val find : 'a t -> key -> 'a
            val find_all : 'a t -> key -> 'a list
            val replace : 'a t -> key -> 'a -> unit
            val mem : 'a t -> key -> bool
            val iter : (key -> 'a -> unit) -> 'a t -> unit
            val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
            val length : 'a t -> int
          end
        module Make :
          functor (H : HashedType) ->
            sig
              type key = H.t
              type 'a t = 'a Hashtbl.Make(H).t
              val create : int -> 'a t
              val clear : 'a t -> unit
              val copy : 'a t -> 'a t
              val add : 'a t -> key -> 'a -> unit
              val remove : 'a t -> key -> unit
              val find : 'a t -> key -> 'a
              val find_all : 'a t -> key -> 'a list
              val replace : 'a t -> key -> 'a -> unit
              val mem : 'a t -> key -> bool
              val iter : (key -> 'a -> unit) -> 'a t -> unit
              val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
              val length : 'a t -> int
            end
        val hash : 'a -> int
        external hash_param : int -> int -> 'a -> int
          = "caml_hash_univ_param" "noalloc"
        val find_noexn : ('a, 'b) t -> 'a -> 'b option
      end
    module List :
      sig
        val length : 'a list -> int
        val hd : 'a list -> 'a
        val tl : 'a list -> 'a list
        val nth : 'a list -> int -> 'a
        val rev : 'a list -> 'a list
        val append : 'a list -> 'a list -> 'a list
        val rev_append : 'a list -> 'a list -> 'a list
        val concat : 'a list list -> 'a list
        val flatten : 'a list list -> 'a list
        val iter : ('a -> unit) -> 'a list -> unit
        val map : ('a -> 'b) -> 'a list -> 'b list
        val rev_map : ('a -> 'b) -> 'a list -> 'b list
        val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
        val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
        val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
        val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
        val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
        val fold_left2 :
          ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
        val fold_right2 :
          ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
        val for_all : ('a -> bool) -> 'a list -> bool
        val exists : ('a -> bool) -> 'a list -> bool
        val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
        val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
        val mem : 'a -> 'a list -> bool
        val memq : 'a -> 'a list -> bool
        val find : ('a -> bool) -> 'a list -> 'a
        val filter : ('a -> bool) -> 'a list -> 'a list
        val find_all : ('a -> bool) -> 'a list -> 'a list
        val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
        val assoc : 'a -> ('a * 'b) list -> 'b
        val assq : 'a -> ('a * 'b) list -> 'b
        val mem_assoc : 'a -> ('a * 'b) list -> bool
        val mem_assq : 'a -> ('a * 'b) list -> bool
        val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
        val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
        val split : ('a * 'b) list -> 'a list * 'b list
        val combine : 'a list -> 'b list -> ('a * 'b) list
        val sort : ('a -> 'a -> int) -> 'a list -> 'a list
        val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
        val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list
        val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
        val assoc_noexn : 'a -> ('a * 'b) list -> 'b option
      end
    val ( ==> ) : 'a -> ('a -> 'b) -> 'b
  end

module Lexer :
  sig
    val trim_f : string -> string
    val to_charlist : string -> char list
    val qtype_of_string : string -> int
    val to_symlist : string -> string list
    val to_bits : string -> int list
    val to_date : string -> Type.q_date
    val to_timestamp : string -> Type.timestamp
    val to_datetime : string -> Type.q_date * Type.timestamp
    val update_loc :
      ?filename:'a option -> Lexing.lexbuf -> int -> int -> unit
    module State :
      sig
        val set_commentable : bool -> unit
        val is_commentable : unit -> bool
        val reset : unit -> unit
        val notify_error : string -> 'a
        module BraceCt :
          sig val incr : unit -> unit val decr : Lexing.position -> unit end
        module BracketCt :
          sig val incr : unit -> unit val decr : Lexing.position -> unit end
        module ParenCt :
          sig val incr : unit -> unit val decr : Lexing.position -> unit end
        val check_parity : unit -> unit
      end
  end
module Parser :
  sig
    val ( ==> ) : 'a -> ('a -> 'b) -> 'b
    val handle_defaults :
      f:(Type.expr -> Type.expr) -> Type.expr -> Type.expr
    val make_dyadic : Type.expr -> Type.expr
    val apply_adverb : Type.expr -> Type.expr
    val make_intlist : Type.expr -> Type.expr
    val make_floatlist : Type.expr -> Type.expr
    val make_datelist : Type.expr -> Type.expr
    val make_timestamplist : Type.expr -> Type.expr
    val make_datetimelist : Type.expr -> Type.expr
    exception Not_table
    val identify_table : Type.expr -> Type.expr
    val identify_lambda_args : Type.lambda -> Lexing.position -> Type.lambda
    val make_sql_clauses : Type.expr -> Type.expr list
    val extract_where_clause : Type.expr -> Type.expr * Type.expr list
  end
