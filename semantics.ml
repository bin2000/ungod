(* A whole bunch of semantics checks. *)

open Type
open Util.Std

(* a function to update the symbol table and the identifier*)
let register_ident symbol_table ident expr =
  Hashtbl.replace symbol_table ident.id_name expr;
  expr
;;

let rec check_assign symbol_table e =
  match e with
  | Apply {fun_id = Apply {fun_id = Builtin {node = node; pos = pos_assign}; arg = e1};
            arg = e2} when node = ":" || node = "::" ->
      begin match e1 with
      | Ident {node = node1; pos = pos1} ->
        begin
          (* A good case. Unifying the assignment to an identifier *)
          match check_assign symbol_table e2 with
          | Ident {node = node2; pos = pos2} as id2 ->
            begin match Hashtbl.find_noexn symbol_table node2.id_name with
            | Some x -> register_ident symbol_table node1 x
            | None ->
              begin
                Error.print_warning pos2
                ((Printf.sprintf "`%s` does not seem to be defined." node2.id_name) ^ 
                  " Is it in a different file or loaded from the disk?");
                id2
              end
            end
          | _ as e3 -> register_ident symbol_table node1 e3
        end
      | _ -> Error.print_error pos_assign "You can only assign to a (user-defined) variable!"
      end 
  | _ -> e

(*
let rec substitute_identifier symbol_table e = 
  match e with
  | Ident {node = {id_name = id_name; id_value: id_value}; pos = pos} ->
      e.node.id_value = Hashtbl.find_noexn symbol_table id_name
  | 
*)

(* gst = global symbol table *)

let rec check_assign_all es =
  let gst = Hashtbl.create 0 in
  begin
    List.map (check_assign gst) es;
  end
;;

let checker = check_assign_all
