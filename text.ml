open Gtree

(* get the bb from the list of attributes, or build the default value
   from the node tag *)
let get_bb_attr t l =
  try
    let Some a = List.find (fun p -> p <> None) l in a
  with Not_found ->
    let w = float_of_int (String.length t) in
    -. w /. 2., w /. 2.

let make_node t la rest = Tree.Node(get_bb_attr t la, t, rest)
let make_leaf t la = Tree.Leaf(get_bb_attr t la, t)

(* we ignore all attrs other than bb *)
let make_attr (n,v) =
  match n, v with
  | "bb", [l;u] -> Some(float_of_string l, float_of_string u)
  | _ , _ -> None

let read_file file =
  let id v = v in
  load_tree id make_attr make_node make_leaf file


(*s UNUSED -- Screen output *)

(* Display a dtree on screen  *)

let display_as_text n p =

  let next_line, print_at_col =
    let col = ref 1 in
    (fun () -> col := 0; print_newline(); print_newline()),
    (fun t c ->
      let l = String.length t in
      let d = max 0 (c-(String.length t)/2 - !col) in
      print_string (String.make d ' '); print_string t; col := !col + d + l)
  in

  let rec make_list n l c =
    match n with
    | Tree.Dleaf(_, t) -> [(l,int_of_float c,t)]
    | Tree.Dnode(_, t,sons) -> (l,int_of_float c,t) ::
	List.flatten (List.map
	  (fun (n,p,v) -> make_list n (l+1) (c+.p))
	  sons)
  in

  let nodes = List.sort
      (fun (l1,c1,_) (l2,c2,_) -> if l1<l2 or l1=l2 && c1<c2 then -1 else 1)
      (make_list n 1 p)
  in

  List.fold_left
    (fun cl (l,c,t) ->
      if l>cl then next_line();
      print_at_col t c; l)
    1
    nodes;
  next_line()
;;
