(* Module for loading and saving generic trees *) 

(* This is the default data structure for trees.  Each node carry a
   tag and a list of attributes.  'v is value (node tag), 'a is
   attribute *)

type ('v,'a) tree =
  | Leaf of 'v * 'a list
  | Node of 'v * 'a list * ('v,'a) tree list

(*
   We'll use a simple text format with one node per line.  Sons of a
   node are indented on the following lines.  Attributes follow their
   node, using the same indentation. For example, the following text
   file:

\begin{verbatim}
one
   two
   three
        four
        five
   six
\end{verbatim}

   is the tree
      [Node ("one", [],
           [Leaf("two"'[]);
            Node ("three", [], 
                [Leaf("four",[]); Leaf("five",[])]);
            Leaf("six",[])])]

   We don't care what the exact number of spaces is, but only that it be
   consistent among brother nodes.  \textbf{Warning}: we want only
   whitespace, tabs will make things go wrong (most probably an error
   will be raised but an incorrect tree could also be built).

   A node's attributes are stored one on each line, on the lines
   following the node, using the node indentation.  They start with a
   ':', then the attribute name, a blank and the attribute value:

   NodeWithoutAttributes
      Son1
      Son2WithAttributes
      :attr1 value1
      :attr2 value2 can include blanks as well
   AnotherNode

*)
      
(*s Text output *)

(* [show_tree] generates the textual representation of the tree.
   [save_tree] saves this in the given file.

   Any tree data structure can be passed to these functions.  The
   [convert_node] parameter should turn any node from such a tree to a
   [tree] value (ie either a [Node] or a [Leaf]).

   pn and pa are [print_node] and [print_attr]: they give a string
   representation of the node label/an attribute.
*)

let show_tree pn pa cn n =
  let at_pos c =
    ("\n" ^ String.make c ' ')
  in
  let print_attrs c attrs =
    String.concat "" @@ List.map (fun a -> at_pos c ^ Printf.sprintf "%s" (pa a)) attrs
  in
  let rec traversal n c =
    (at_pos c ^
     match n with
     | Leaf(t,a) -> Printf.sprintf "%s" (pn t) ^ print_attrs c a
     | Node (t, a, sons) ->
        (Printf.sprintf "%s" (pn t) ^ print_attrs c a ^
	   String.concat "" @@ List.map (fun n -> traversal n (c + 4)) sons))
  in
  traversal (cn n) 0 ^ "\n"

let print_tree pn pa cn oc n =
  let at_pos c =
    (output_char oc '\n'; output_string oc (String.make c ' '))
  in
  let print_attrs c attrs =
    List.iter (fun a -> at_pos c; Printf.fprintf oc "%s" (pa a)) attrs
  in
  let rec traversal n c =
    (at_pos c;
     match n with
     | Leaf(t,a) -> Printf.fprintf oc "%s" (pn t); print_attrs c a
     | Node (t, a, sons) ->
        (Printf.fprintf oc "%s" (pn t); print_attrs c a;
       List.iter (fun n -> traversal n (c + 4)) sons))
  in
  traversal (cn n) 0; output_char oc '\n'
  
let save_tree pn pa cn t name =
  let oc = open_out name in (print_tree pn pa cn oc t; close_out oc)

(*s Text input *)

(*s Lexical analysis *)

(* First some helper functions *)
  
(* [indent_size] returns the position of first non space character.
   It throws [Invalid_argument] if none exists. *)

let indent_size st =
  let rec is n = if (String.get st n) <> ' ' then n else is (n + 1) in is 0

(* Simple helper: get substring of s starting at position p. *)
let string_sub s p =
  String.sub s p (String.length s - p)

(* This one could/should exist in a general purpose library. *)
(* Well, Str.split (Str.regexp sep) s does it *)
let string_split sep s =
  let rec help s acc =
    try
      let i = String.index s sep in
      help (string_sub s (i+1)) (String.sub s 0 i :: acc)
    with Not_found ->
      List.rev (s :: acc)
  in
  help s []
;;
(* [string_sub] 1 to remove the leading colon *)
let split s = let (a::v) = string_split ' ' (string_sub s 1) in (a,v)

(* Here is the type of tokens, and the ``lexical analyser'' function,
   that produces a stream of tokens from the file text.
*)

type token = Text of string | Attr of string * string list | Begin | End

let lex ic =
  let s = Stack.create () 
  and bl = ref []  (* backlog *)
  and n = ref 0 in

  (* read the next line and return the pair (position, text);
     if no text (blank line) or comment, try next line *)
  
  let rec next_line () =
    (n := !n + 1;
     let l = input_line ic in
     if String.length l > 0 && l.[0] = '#' then next_line() else
       try
	 let i = indent_size l
	 in (i, (String.sub l i ((String.length l) - i)))
       with | Invalid_argument _ -> next_line ()) in
  
  (* This is the function that produces tokens for the stream.
     Several tokens can be produced in one invocation, so a local backlog
     queue is set up.
     
     The only possible parse error is of a unaligned left text ZZZ 
  *)
  let rec next _ =
    if !bl <> []
    then (* if there's backlog, use it *)
      let r = List.hd !bl in (bl := List.tl !bl; Some r)
    else (* otherwise read from the file *)
      try
        let (i, t) = next_line ()
        in
	if t.[0] = ':' then
	  let (a,v)=split t in Some (Attr(a,v))
	else
          if i = Stack.top s
          then (* text at same level *) Some (Text t)
          else
            if i > Stack.top s then begin  (* more indentation *)
              Stack.push i s;
              bl := [ Text t ];
              Some Begin
	    end else begin   	           (* less indentation *)
              bl := [ Text t ];
              while Stack.top s <> i do (* count the ZZZ *)
                bl := End :: !bl; Stack.pop s done;
              next 0
	    end
      with End_of_file -> Some End
      | Stack.Empty -> failwith ("parse error line " ^ (string_of_int !n))
  in
  Stack.push 0 s;
  Stream.from next
  
(*s Syntax analysis *)

(* The grammar for trees is:

\begin{verbatim}
   T -> t A R
   A -> attr A | eps
   R -> begin T S end | eps
   S -> T S | eps
\end{verbatim}

We use caml streams, available from the [camlp4o] extension and our
parser is then trivial (thanks to our grammar designed to be
LL(1)).

Any tree data structure can be built with this parser.
The parameters:

   - [pn] is a parsing function for a node tag.  It returns a node tag
     from the string in the text file,

   - [pa] returns an attribute value from a pair of string (attr name,
     attr value)

   - [mn] and [ml] (for make node/leaf) build non leaf and leaf
     nodes.
 *)

let rec tree pn pa mn ml = parser
  | [< 'Text(t); a=attrs pa; r=rest pn pa mn ml >] ->
     if r=[] then ml (pn t) a else mn (pn t) a r
and attrs pa = parser
    | [< 'Attr(a,v); avs=attrs pa >] -> (pa (a,v))::avs
    | [<>] -> []   
and rest pn pa mn ml = parser
  | [< 'Begin; s=tree pn pa mn ml; ls=sons pn pa mn ml [s]; 'End >] -> ls
  | [< >] -> []
and sons pn pa mn ml ls = parser
  | [< s=tree pn pa mn ml; ls=sons pn pa mn ml (ls @ [s]) >] -> ls
  | [< >] -> ls
  
let load_tree pn pa mn ml name =
  let ic = open_in name in
  let t = tree pn pa mn ml (lex ic) in
  close_in ic; t

    
(*s Example: save/load a (string, (string * string) list) tree *)

let save_tree_string t name =
  let id v = v
  and pr (a,v) = Printf.sprintf "%s %s" a v
  in save_tree
  id (* the tag is its own printed representation *)
  (fun l -> String.concat "" @@ List.map pr l) (* ZZZ? *)
  id (* for a [tree] so no node conversion *)
  t name

let make_leaf t a = Leaf(t,a)
let make_node t a rest = Node(t,a,rest)
  
let load_tree_string name =
  let id v = v in
  load_tree id id make_node make_leaf name
