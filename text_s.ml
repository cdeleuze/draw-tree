open Tree

(*s Text input/output *)

(* We want to be able to load and save trees on files.  We'll use a
   simple text format with one node per line.  Sons of a node are
   indented on the following lines.  For example, the following text file:

\begin{verbatim}
one
   two
   three
        four
        five
   six
\end{verbatim}

   is the tree [Node ("one",
   [Leaf "two"; Node ("three", [Leaf "four"; Leaf "five"]); Leaf "six"])]

We don't care what the exact number of spaces is, but only that it be
consistent among brother nodes.  \textbf{Warning}: we want only
whitespace, tabs will make things go wrong (most probably an error
will be raised but an incorrect tree could also be built).

*)

(*s Text output *)

(* [print_tree] generates the textual representation of the tree.
   [save_file] saves this in the given file. *)

let print_tree oc n =
  let at_pos c = output_char oc '\n' ; output_string oc (String.make c ' ')
  in
  let rec traversal n c =
    at_pos c;
    match n with
    | Leaf ((l,r),t) -> Printf.fprintf oc "%s<%f;%f>" t l r
    | Node((l,r),t, sons) ->
	Printf.fprintf oc "%s<%f;%f>" t l r;
	List.iter (fun n -> traversal n (c+4)) sons
  in
  traversal n 0;
  output_char oc '\n'
;;

let save_file t name =
  let oc = open_out name in
  print_tree oc t;
  close_out oc
;;


(*s Text input *)

(* We read a tree from a horizontal indented text file.  On each line,
   we'll get an indent value, a text and possibly a left and right
   extent.  ``Lexical analysis'' will produce a stream of tokens
   representing nodes and begin/ends for sons lists.  begin/end tokens
   will be generated from the indentation.

   Then, ``syntax analysis'' will turn the stream of tokens into a
   [string tree].
  *)

(* [indent_size] returns the position of first non space character.
   It throws [Invalid_argument] if none exists. *)

let indent_size st =
  let rec is n =
    if String.get st n <> ' ' then n
    else is (n+1)
  in
  is 0
    

(* Here is the type of tokens, and the ``lexical analyser'' function, that produces a stream
   of tokens from the file text.
*)

type token = Text of string | Begin | End

let lex ic =
  let s = Stack.create () 
  and bl = ref []  (* backlog *)
  and n = ref 0 in
  
  (* read the next line and return the position pair (position, text)
     if no text (blank line), try next line *)
  
  let rec next_line () =
    n := !n + 1;
    let l = input_line ic in
    try
      let i = indent_size l in
      i, String.sub l i (String.length l - i)
    with Invalid_argument _ -> next_line ()
  in
  
  (* This is the function that produces tokens for the stream.
  Several tokens can be produced in one invocation, so a local backlog
  queue is set up.
     
  The only possible parse error is of a unaligned left text ZZZ 
  *)
  
  let rec next _ =
    if !bl <> [] then begin   (* if there's backlog, use it *)
      let r = List.hd !bl in
      bl := List.tl !bl;
      Some (r)
    end else                  (* otherwise read from the file *)
      try
	let i,t = next_line ()
	in
	if i = Stack.top s then        (* text at same level *)
	  Some(Text(t))
	else
	  if i>Stack.top s then begin  (* text on right *)
	    Stack.push i s;
	    bl := [ Text(t) ];
	    Some(Begin)
	  end
	  else begin                   (* text on left *)
	    bl := [ Text(t) ];
	    while Stack.top s <> i do  (* count the ZZZ *)
	      bl := End :: !bl;
	      Stack.pop s
	    done;
	    next 0
	  end
      with End_of_file -> Some(End)
      | Stack.Empty -> failwith ("parse error line " ^ (string_of_int !n))
  in
  Stack.push 0 s;
  Stream.from next
;;

(*s Syntax analysis *)


(* The grammar for trees is:

\begin{verbatim}
   T -> t R
   R -> begin T S end | eps
   S -> T S | eps
\end{verbatim}

We use caml streams, available from the [camlp4o] extension and our
parser is then trivial (thanks to our grammar designed to be
LL(1)). *)

let make_node rest t =
  let (s,(l,r)) = 
    if t.[String.length t - 1] = '>' then
      let i = String.rindex t '<' in
      String.sub t 0 i,
      Scanf.sscanf (String.sub t i (String.length t - i)) "<w=%f" (fun l -> (-.l/.20.,l/.20.))
    else
      t,
      let w = float_of_int (String.length t) in
      -. w /. 2., w /. 2.
  in
  if rest = [] then Leaf((l,r),s) else Node((l,r),s,rest)

let rec tree = parser
  | [< 'Text(t); r=rest >] -> make_node r t
and rest = parser
  | [< 'Begin; s=tree; ls=sons [s]; 'End >] -> ls
  | [< >] -> []
and sons ls = parser
  | [< s=tree; ls=sons (ls @ [s]) >] -> ls
  | [< >] -> ls

let read_file name =
  let ic = open_in name in
  let t = tree (lex ic)
  in close_in ic; t
;;


(*s Screen output *)

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
    | Dleaf(_, t) -> [(l,int_of_float c,t)]
    | Dnode(_, t,sons) -> (l,int_of_float c,t) :: 
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
