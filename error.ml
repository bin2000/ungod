open Printf

exception Semantic_error

let print_warning pos msg =
  begin
    Location.print_loc pos;
    print_endline (sprintf "WARN: %s" msg)
  end

let print_error pos msg =
  begin
    Location.print_loc pos;
    print_endline (sprintf "ERROR: %s" msg);
    raise Semantic_error
  end
