open Gtree

(* How to read a tree from a text file.  This makes use of load_tree
   from the tree_io/gtree module for generic tree loading/saving.

   In addition to the name of the file to read from, load_tree is to be
   given functions to build elements of the tree from the parsed text:
    - make_label :: make node label from string label
    - make_attr  :: make attribute from (name, [values])
    - make_node  :: make non leaf node
    - make_leaf  :: make leaf node
 *)

(* the BB attribute is special as it will be used in the drawing *)
type attr = BB of float*float
          | Attr of string * string list

let show_attr = function
  | BB(l,r) -> Printf.sprintf ":bb %f %f" l r
  | Attr(name, vals) -> Printf.sprintf ":%s %s" name @@ String.concat " " vals

(* display an attribute, ie produce a string to be used in the picture *)
let display_attr = function
  | Attr(name, vals) -> Printf.sprintf "%s=%s" name @@ String.concat " " vals
  | BB(l,r) as a -> show_attr a

(* get the bb from the list of attributes, or build the default value
   from the node tag *)
let get_bb_attr t l =
  try
    let BB(l,r) = List.find (fun p -> match p with BB _ -> true | _ -> false) l in (l,r)
  with Not_found ->
    let w = float_of_int (String.length t) in
    -. w /. 2., w /. 2.

(* TODO: should remove BB attribute from the list? *)
let make_node t la rest = Tree.Node(get_bb_attr t la, (t,la), rest)
let make_leaf t la = Tree.Leaf(get_bb_attr t la, (t,la))

(* read an attribute *)
let make_attr (n,v) =
  match n, v with
  | "bb", [l;u] -> BB(float_of_string l, float_of_string u)
  | _ , _ -> Attr(n,v)

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
      let t = fst t in
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
