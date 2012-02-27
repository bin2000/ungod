open Parsing
open Lexing
open Printf
open Type

let print_loc p = 
  if p.pos_fname = "" then
    printf "<%i,%i>" p.pos_lnum p.pos_bol
  else
    printf "<%s %i,%i>" p.pos_fname p.pos_lnum p.pos_bol

(* this function finds the location of the expression. *)
let rec spot_loc e =
  match e with 
  | Ident x -> x.pos
  | Builtin x -> x.pos
  | Atom x -> x.pos 
  | List x | Block x -> x.pos
  | Lambda x -> x.pos
  | Protected x -> spot_loc x
  | ModifiedDyadic {orig = x; modifier = _} -> spot_loc x
  | Apply {fun_id = x; arg = _} -> spot_loc x
  | Sql s -> s.header.pos
  | Table t ->
      begin match t.columns with
      | [] -> failwith "a table must have at least one column!"
      | hd :: _ -> spot_loc hd
      end
  | Empty -> failwith "Empty should appear only inside a list!"
