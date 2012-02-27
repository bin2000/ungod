module Std = struct
  module Hashtbl = struct
    include Hashtbl
    let find_noexn h k = 
      try Some (find h k)
      with e -> None
  end

  module List = struct
    include List
    let assoc_noexn elt lst =
      try Some (assoc elt lst)
      with _ -> None
  end

  let (==>) f g = g f
end


module Lexer = struct
  let trim_f s = 
    match s with
    | "" -> ""
    | _ -> begin
      match s.[(String.length s) - 1] with
      | 'f' -> String.sub s 0 ((String.length s) - 1)
      | _ -> s
    end

  let to_charlist s =
    let l = ref [] in
    for i = (String.length s) - 2 downto 0 do
      l := s.[i] :: !l
    done;
    !l
 
  let qtype_of_string s =
    let n = String.sub s 0 ((String.length s) - 1) in
    try
      int_of_string n
    with e -> assert false

  let to_symlist s =
  (* take an input of the form "`x`y`z" and converts it to [`x; `y; `z] *)
    let n = String.length s in
      assert (0 < n && s.[0] = '`');
      let rec loop i buf lst =
        if i >= n then 
          List.rev (buf :: lst)
        else begin
          match s.[i] with
          | '`' -> loop (i + 1) "" (buf :: lst)
          | ' ' | '\n' | '\t' ->
            failwith "ERROR: we should not be seeing a whitespace"
          | c -> loop (i + 1) (buf ^ String.make 1 c) lst
        end in
      loop 1 "" []
    
  let to_bits s = 
    let n = String.length s in
    begin
      assert ((1 < n) && s.[n - 1] = 'b');
      let l = ref [] in
      for i = 0 to n - 2 do
        match s.[i] with
        | '0' -> l := 0 :: !l
        | '1' -> l := 1 :: !l
        | _ -> failwith "ERROR: a bit list only accepts 0/1!"
      done;
      List.rev !l;
    end
  ;;


  let to_date s =
    match Str.split (Str.regexp "\\.") s with
    | [yr; mo; dt] -> {
          Type.year = int_of_string yr;
          Type.month = int_of_string mo;
          Type.date = int_of_string dt;
       }
    | _ -> failwith ("ERROR: failed to parse " ^ s ^ " as a date!")
  ;;

  let to_timestamp s =
    match Str.split (Str.regexp "[\\.:]") s with
    | [hh; mm; ss] -> {
          Type.hour = int_of_string hh;
          Type.min = int_of_string mm;
          Type.sec = int_of_string ss;
          Type.milli = 0;
       }
    | [hh; mm; ss; mmm] -> {
          Type.hour = int_of_string hh;
          Type.min = int_of_string mm;
          Type.sec = int_of_string ss;
          Type.milli = int_of_string mmm;
       }
    | _ -> failwith ("ERROR: failed to parse " ^ s ^ " as a timestamp!")
  ;;

  let to_datetime s = 
    match Str.split (Str.regexp "T") s with
    | [x; y] ->
        (to_date x), (to_timestamp y)
    | _ -> failwith ("ERROR: failed to parse " ^ s ^ " as a datetime!")
  ;;


  open Lexing

  let update_loc ?(filename = None) lexbuf line_incr cursor_incr =
    let p = lexbuf.lex_curr_p in
    let cursor_pos = if line_incr <> 0 then 0 else p.pos_bol in
    let new_p = {
      pos_fname = p.pos_fname;
      pos_lnum = p.pos_lnum + line_incr;
      pos_bol = cursor_pos + cursor_incr;
      pos_cnum = p.pos_cnum; (* I am not using this for now *)
   } in 
    lexbuf.lex_curr_p <- new_p;
  ;;



  module State = struct
    let brace_ct = ref 0
    let bracket_ct = ref 0
    let paren_ct = ref 0
    let commentable = ref true
    
    let set_commentable flag =
      commentable := flag

    let is_commentable () = !commentable
    
    let reset () = begin
      brace_ct := 0;
      bracket_ct := 0;
      paren_ct := 0;
      commentable := true;
    end

    let notify_error err_msg =
      begin
        reset ();
        failwith err_msg;
      end

    module Counter (S : sig
      val retrieve : unit -> int ref
      val end_char : char
    end) = struct
      let incr () = incr (S.retrieve ())
      let decr position =
        let ct = S.retrieve () in
        if !ct > 0 then
          decr ct
        else begin
          reset ();
          Location.print_loc position;
          notify_error (Printf.sprintf "mismatched %c" S.end_char)
        end
    end

    module BraceCt = Counter (struct
      let retrieve () = brace_ct
      let end_char = '}'
    end)

    module BracketCt = Counter (struct
      let retrieve () = bracket_ct
      let end_char = ']'
    end)

    module ParenCt = Counter (struct
      let retrieve () = paren_ct
      let end_char = ')'
    end)

    let check_parity () =
      match !brace_ct, !bracket_ct, !paren_ct with
      | 0, 0, 0 -> ()
      | 0, 0, _ -> reset (); notify_error "mismatched ("
      | 0, _, 0 -> reset (); notify_error "mismatched ["
      | _, 0, 0 -> reset (); notify_error "mismatched {"
      | _, _, _ -> reset (); notify_error "One of (,[ or {was not closed"
    ;;
    
  end
  ;;

end

module Parser = struct
  open Type

  let (==>) = Std.(==>)

  (* A function to deal with braindead cases of expression transformation.
   * ~f is the transformation function *)
  let handle_defaults ~f e =
    match e with
    | Apply _ -> failwith "the Apply type should not reach here!"
    | Protected e' -> f e'
    | Lambda l ->
        let args' = List.map f l.node.lambda_args in
        let body' = List.map f l.node.lambda_body in
        Lambda {node = {lambda_args = args'; lambda_body = body'}; pos = l.pos}
    | List l ->
        List {node = List.map f l.node; pos = l.pos}
    | Block l ->
        Block {node = List.map f l.node; pos = l.pos}
    | Table {key_defs = k; columns = c} ->
        Table {key_defs = List.map f k; columns = List.map f c}
    | Sql {header = h; selector = s; by = bys; table = t; where = wheres} ->
        begin
          let s' = List.map f s in
          let t' = f t in 
          let bys' = List.map f bys in
          let wheres' = List.map f wheres in
          Sql {header = h; selector = s'; by = bys'; table = t'; where = wheres'}
        end
    | Ident _ | Builtin _ | Atom _ | Empty | ModifiedDyadic _ -> e

  
  let dyadic_builtins = ["+"; "-"; "*"; "%"; "#"; "&"; "$"; "_"; "~"; "^"; ",";
                         "<"; ">"; "="; ":"; "::"]
  let is_dyadic builtin = List.mem builtin dyadic_builtins

  let dyadic_adverbs = ["/"; "\\"; "\\:"; "/:"]
  
  let is_dyadic_adverb builtin = List.mem builtin dyadic_adverbs


  let rec make_dyadic e =
    (* If app = e1 builtin e2, instead of applying builtin and e2
     * to e1, make it (builtin e1) e2 *)
    match e with
    | Apply {fun_id = e1; arg = e2(**)} ->
        begin match e2 with
          | Builtin builtin as b when is_dyadic builtin.node ->
              Apply {fun_id = b; arg = e1}
          | ModifiedDyadic _ -> Apply {fun_id = e2; arg = e1}
          | Apply {fun_id = e3; arg = e4} when
              begin
                match e3 with
                | Builtin builtin -> is_dyadic builtin.node
                | ModifiedDyadic _ -> true
                | _ -> false
              end ->
              let e1' = make_dyadic e1 in
              let e4' = make_dyadic e4 in
              Apply {fun_id = Apply {fun_id = e3; arg = e1'}; arg =e4'}
          | _ ->
              Apply {fun_id = make_dyadic e1; arg = make_dyadic e2}
        end
    | _ -> handle_defaults ~f:make_dyadic e

  let rec apply_adverb e =
    (* This function applies adverbs, i.e., if it sees something like `+/`, it
     * groups them together and create a new entity *)
    match e with
    | Apply {fun_id = e; arg = Builtin b} when is_dyadic_adverb b.node -> 
        ModifiedDyadic {orig = apply_adverb e; modifier = b.node}
    | Apply {fun_id = e1; arg =
      Apply {fun_id = Builtin b; arg = e2}} when is_dyadic_adverb b.node ->
        apply_adverb (Apply {
          fun_id = ModifiedDyadic {orig = apply_adverb e1; modifier = b.node};
          arg = e2
        })
    | Apply {fun_id = e1; arg = e2} ->
        Apply {fun_id = apply_adverb e1; arg = apply_adverb e2}
    | _ -> handle_defaults ~f:apply_adverb e

  let make_shorthand typecheck =
    let rec shorthand e =
      match e with
      | Apply {fun_id = Atom a; arg = List {node = []; pos = _}} ->
          begin
            Location.print_loc a.pos;
            failwith "You cannot call an atom!"
          end
      | Apply {fun_id = Atom a1; arg = e'} when typecheck a1 ->
          begin
            let pos = a1.pos in
            match shorthand e' with
            | Atom a2 when typecheck a2 ->
                List {node = [Atom a1; Atom a2]; pos = pos}
            | List l  ->
                List {node = (Atom a1) :: l.node; pos = pos}
            | Apply {fun_id = Atom a2; arg = arg'} when typecheck a2 ->
                Apply {fun_id = List {node = [Atom a1; Atom a2]; pos = pos};
                        arg = arg'}
            | Apply {fun_id = List l; arg = arg'} ->
                Apply {fun_id = List {node = (Atom a1) :: l.node; pos = pos}; 
                        arg = arg'}
            | _ as e'' ->
                Apply {fun_id = Atom a1; arg = e''}
          end
      | Apply {fun_id = e1; arg = e2} ->
          let e2' = shorthand e2 in
          let e1' = shorthand e1 in
          Apply {fun_id = e1'; arg = e2'} 
      | _ -> handle_defaults ~f:shorthand e in
    shorthand
  ;; 

  let make_intlist =
    make_shorthand (function {node = Int _; pos = _} -> true | _ -> false)

  let make_floatlist =
    make_shorthand (function {node = Float _; pos = _} -> true | _ -> false)

  let make_datelist =
    make_shorthand (function {node = Date _; pos = _} -> true | _ -> false)

  let make_timestamplist =
    make_shorthand (function {node = Timestamp _; pos = _} -> true | _ -> false)

  let make_datetimelist =
    make_shorthand (function {node = Datetime _; pos = _} -> true | _ -> false)


  (*This function is supposed to recognize a table from a list*)
  exception Not_table

  let identify_table e =
    let rec aux e =
      match e with
      | Protected (Block b) ->
          Error.print_error b.pos "([...]) is not a legitimate table construct."
      | Protected (Apply {fun_id = Block b; arg = e}) ->
          {key_defs = b.node; columns = [e]}
      | Protected e' -> aux e'
      | List {node = hd :: tl; pos = p}->
          begin match hd with
          | Apply {fun_id = Block b'; arg = e'} ->
              {key_defs = b'.node; columns = e' :: tl}
          | Block b ->
            (*this is the case that looks like ([];...), which is not legit*)
              Error.print_error b.pos "([];...) is not a legitimate table construct."
          | _ -> raise Not_table
          end
      | _ -> raise Not_table in
    try Table (aux e)
    with Not_table -> e
  ;;

  let identify_lambda_args la pos =
    assert (la.lambda_args = []); (* coming out of parse there should not be args*)
    let default_args pos = List.map (fun a ->
      Ident {node = {id_name = a; id_value = None}; pos = pos})
      ["x"; "y"; "z"] in
    match la.lambda_body with
    | [] -> {lambda_args = default_args pos; lambda_body = []}
    | hd :: tl ->
        begin match hd with
        | Block b ->
            {lambda_args = b.node; lambda_body = tl}
        | Apply {fun_id = Block b; arg = e} ->
            {lambda_args = b.node; lambda_body = e :: tl}
        | _  ->
            {lambda_args = default_args pos; lambda_body = la.lambda_body}
        end
  ;;
    

  let rec push_thru_application app term =
    match app with
    | Empty -> term
    | Apply {fun_id = e1; arg = e2} ->
        Apply {fun_id = e1; arg = push_thru_application e2 term}
    | _ -> Apply {fun_id = app; arg = term}

  let make_sql_clauses e = 
    let rec loop curr acc rest =
      match rest with
      | Empty -> List.rev (curr :: acc)
      | Apply {fun_id = Builtin {node = ","; pos = pos}; arg = e} ->
          if curr = Empty then
            Error.print_error  pos "A misplaced ',' in a sql-like statement."
          else
            loop Empty (curr :: acc) e
      | Apply {fun_id = _; arg = Builtin {node = ","; pos = pos}} ->
          Error.print_error  pos "A misplaced ',' in a sql-like statement."
      | Apply {fun_id = e1; arg = e2} ->
          loop (push_thru_application curr e1) acc e2
      | _ ->
          loop (push_thru_application curr rest) acc Empty in
    loop Empty [] e

  let extract_where_clause e =
    match e with 
    | Apply {fun_id = e1;
              arg = Apply {fun_id = Ident {node = {id_name = "where"; id_value = _};
                                             pos = _};                                   
                            arg = e2}}->
        if e2 = Empty then
          failwith "ERROR: an empty where clauses in a sql-like statement"
        else
          (e1, make_sql_clauses e2)
    | e' -> (e', [])
        
end
