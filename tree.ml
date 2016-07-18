(*p

\usepackage{a4wide}
\usepackage{graphicx}
\usepackage{float}
\usepackage{url}
\usepackage{subfigure}
\newcommand{\bibfieldurl}[1]{ Page web : {\small\url{#1}}.}

%% TODO edge pour bord des noeuds, et les liens ?

\newcommand{\img}[2]{\includegraphics[width=#1]{#2.eps}}
\newcommand{\imgl}[2]{\includegraphics[angle=90,height=#1]{#2.eps}}
\newcommand{\sub}[2]{\subfigure[#1]{\img{.49\linewidth}{#2}}}
\newcommand{\subs}[3]{\subfigure[#1]{\img{#2\linewidth}{#3}}}
\newenvironment{figureH}{\begin{figure}[h]}{\end{figure}}
\newenvironment{figureh}{\begin{figure}[h]}{\end{figure}}

\title{\vspace{-3cm}How to draw nice trees}
\author{Christophe Deleuze}
\AtBeginDocument{\maketitle

\begin{abstract} 

This OCaml litterate program module is part of a program whose purpose
is to make graphical representations of trees, drawn on screen or
saved as encapsulated postscript files.  This module implements the
algorithms that set the horizontal positions of nodes based on various
policies.  It also includes strategies to obtain compact
representations.

\end{abstract}}
*)

(*s Type declarations *)


(*

 We'll use a simple recursive definition for trees.  Each node holds a
bounding box describing the space needed for displaying its label.
For now, we define the bounding box as holding only horizontal extent.
Future versions may use a real bounding box also holding vertical
extent.  We won't use the label itself when computing node positions
so it is of an unknown type ['a].  *)

type bb_t = float * float

type 'a tree =
  | Leaf of bb_t * 'a
  | Node of bb_t * 'a * ('a tree list)
	
(*

The complexity of the solution to a problem often depends on the
choice of a good data structure.  Although we didn't get this on the
first thought, here we define a drawn tree as being either a leaf node
or a node with a list of sons, each son having a position relative to
its father, as a horizontal offset and a vertical offset.  What's good
here is that only local information is stored, fitting well to the
recursive definition of a tree (storing a position relative to the
root would turn out to be awkward.) *)

type 'a dtree =
  | Dleaf of bb_t * 'a
  | Dnode of bb_t * 'a * ( ('a dtree * float * float) list )


(*s A first tree drawing algorithm *)

(*

Since node positions are relative to their father, we can easily
assemble sub[dtree]s recursively.  Suppose we have a node with n
subtrees, and we already computed the [dtree]s for all of them.  All
we need to build the global [dtree] is to set the position of
each subtree root node.

How can we determine such positions?  We'll use what we call
``shapes''.  Each node has a horizontal extent given in its bounding
box.  We'll call this \emph{width} in the following, although it's
actually a pair of floats.  Each subtree has a shape which is built
from the widths of all its nodes.  There are various ways to define
the shape.  In this first algorithm we will use the rectangular area
whose width is the maximum width taken by brother nodes in the
subtree.  Another possibility we consider in our second algorithm is
to build an ``envelope'' from the widths of all the nodes.

So our recursive algorithm will take a [tree] and return a [dtree] and
its shape.  The recursive definition of a tree says that a tree [t] is
either a leaf [l] or a node [n] with some trees as sons.  The
algorithm thus considers both cases :

\begin{itemize}

\item the leaf node tree.  The [dtree] is trivial, the shape is built
  from [l]'s width.

\item the node [n] with one or more subtrees.  From [n]'s width and
  the shapes of its subtrees we compute the positions and the shape of
  [t].

\end{itemize}

In the second case, we need to compute positions of the sub[dtrees],
which comprises two distinct problems.  The first one is to compute
the distance between sub[dtree]s according to their shapes, while the
second is to set their relative position to the root node [n].  Most
of the time we want the root node to be centered somewhat (there are
several possible definitions of centering) but we may also want to
draw a tree ``flushed left''.

The parameters of our algorithm will be:

\begin{itemize}
\item[v] the vertical offset between nodes (we'll consider this is a
constant),
\item[sep] the minimum space required between adjacent nodes,

\item[pos_root] a function returning the position of the root of a
tree, given the the root node and the positions and shapes of the
subtrees.  This will implement the chosen centering policy.
\end{itemize}

The basic components used will be:
\begin{itemize}
\item[leaf_shape] a function returning the shape of a leaf node,

\item[positions] a function that takes a list of [dtree]s and
  their shapes and computes the position of all [dtree]s relative to
  their root.

\item[merge_shapes] a function that computes the shape of a set of
  subtrees from the root node and the subtrees positions and shapes,

\item[cons_shape] a function taking a root node and the merged shape of
  its subtrees and returning the tree shape.
\end{itemize}

The shape of a tree will be the rectangular area occupied by all its
nodes. Thus each subtree will monopolize a rectangular area on the
screen.  Since the shapes can not overlap, the tree may be widest than
strictly necessary, but has alignement properties that may be what we
want in some cases.

So the shape is basically a width, but remember we call width a pair
of floats representing left and right extent.  We'll note the shape as
a pair of floats who are the extension of the shape on the left and on
the right on the root node.  In this way, all information about a
subtree is relative to the root node, that is considered at position
0.

The algorithm then reads as follows:
*)

let width n =
  match n with
  | Leaf ((l,r),_) -> (l,r)
  | Node ((l,r), _ , _) -> (l,r)


let dtree_of_tree v sep pos_root t =
  
  (* The shape of a leaf node: the position is the center of the node,
     by definition.*)
  let leaf_shape n = width n
  in

  (* The [cons]ed[_shape] is built from the root width and the merged
    shapes of the subtrees. *)

  let cons_shape n (l,r) = 
    let (ln,rn) = width n in min l ln, max r rn
  in

  (* Get the positions of the subtrees, the first one being placed
  arbitrarily at position 0 (this amounts to computing the distance
  between subtrees). *)

  let positions dsts shs =
    match shs with
    | (l1,r1)::shs ->
	(* Compute the distance between adjacent subtrees. *)
	let dists, _ = List.fold_left 
	    (fun (ds,r) (l',r') -> ds @ [ r+.sep-.l' ], r')
	    ([0.],r1)
	    shs
	in
	(* Fold that in a distance to the first tree. *)
	let pos,_ = 
	  List.fold_left (fun (ps,p) d -> ps @ [p+.d], p+.d) ([],0.) dists
	in pos
    | _ -> failwith "pos"
      
  in

  let merge_shapes pos shs =
    let pn = List.hd (List.rev pos)
    in
    match shs, List.rev shs with
    | (l1,_)::_, (_,rn)::_ -> (l1, pn+.rn)
    | _ -> failwith "merge_shapes"
	  
  in
  
  (* Now the main body of the algorithm. *)

  let rec dt_of_t t =

    match t with
    | Leaf (bb,l) -> Dleaf (bb,l), leaf_shape t
	  
    | Node(bb, lb, subts) ->
	  (* first get the dtrees and shapes of all subtrees *)
	  let dsts, shs = List.split (List.map dt_of_t subts)
	  in
	  (* get relative subtrees positions *)
	  let pos = positions dsts shs in
	  (* shape of concatenated subtrees *)
	  let (l,r) = merge_shapes pos shs in
	  (* position of root *)
	  let root = pos_root pos subts shs (l,r) t
	  in
	  let pos = List.map (fun p -> p -. root) pos in
	  let (l,r) = l-.root, r-.root
	  in
	  (* return global dtree and shape *)
	  Dnode(bb,lb, List.map2 (fun dst p -> dst,p,v) dsts pos),
	  (cons_shape t (l,r))
  in
  let dt,_  = dt_of_t t in dt
    

(*s Root centering *)

(* How do we center, or more generally position roots? We'll define
three methods for centering, illustrated on Figure \ref{fig:center}:

\begin{enumerate} 
\renewcommand{\theenumi}{\alph{enumi}}

\item Use the positions of extreme nodes.  The edges leading to those
  nodes will have symmetric slopes.

\item Use the total width of the concatenated subtrees.  The root will
  be at the center of the space occupied by the tree.

\item Use the distance between the left edge of the left node and
  the right edge of the right node.  Somewhat ``intermediate''
  possibility.
\end{enumerate}

\begin{figureh}
  \centering
  \sub{Position centering}{centerp}\hfill
  \sub{Shape centering}{centers}
  \sub{Node centering}{centern}
  \caption{Centering policies}
  \label{fig:center}
\end{figureh}

We may also want to draw flushed trees (Figure \ref{fig:flush}),
aligning:
\begin{enumerate}
\item positions (ie centers) of left (or right) nodes,
\item left (or right) edges of nodes.
\end{enumerate}


\begin{figureh}
  \centering
  \sub{Left on position}{leftp}\hfill
  \sub{Right on position}{rightp}
  \sub{Left on node edges}{leftn}\hfill
  \sub{Right on node edges}{rightn}
  \caption{Flush left and right policies}
  \label{fig:flush}
\end{figureh}

One can imagine other possibilities.  However, we note that only
context independent methods are possible here, ie we decide of the
centering with information pertaining to the current tree only. 

%  Or maybe there are some tricks to achieve this? We'll see later.

% ZZZ passer une information récursivement ? open recursion ? continuation ?


The following functions may be given as value for the [pos_root]
parameter and implement the centering policies just discussed.

  *)


let root_centerp pos sbts shs sh t =
  match List.rev pos with
  | pn :: _ -> pn /. 2.

let root_leftp pos sbts shs sh t =
  0.

let root_rightp pos sbts shs sh t =
  match List.rev pos with
  | pn :: _ -> pn

let root_centers pos sbts shs sh t =
  match sh with (l,r) -> (l+.r)/.2.

let root_centern pos sbts shs sh t =
  match sbts, List.rev sbts, List.rev pos with
  | n1::_, nn::_, pn::_ -> 
      let l1,r1 = width n1 and ln,rn = width nn in
  (l1 +. ln +. pn) /. 2.

let root_leftn pos sbts shs sh t =
  match sbts, List.rev sbts with
  | n1::_, nn::_ -> 
      let (l1,r1) = width n1 and (lt,rt) = width t in
      (l1 +. rt)
  
let root_rightn pos sbts shs sh t =
  match sh with
  | _,r -> 
        let (lt,rt) = width t in (r +. lt)


(*s Mixing policies *)

(* 

   If one wants to mix various centering policies, it can do so by
   encoding information in each node along with the label. The
   [pos_root] function would then implement a policy chosen based on
   the information found in the node.
   ZZZ
*)


(*s Aligning leaves *)

(*

Sometimes, we want all leaves to be aligned.  We can do this easily by
changing the vertical offset of each leaf so that they are all at the
same vertical position.  We start by computing the vertical position
of the ``lowest'' leaf:

*)

let max_v t =
  let rec max_v t v =
    match t with
    | Dleaf _ -> v
    | Dnode(_,_,l) ->
	List.fold_left (fun acc (t',hp,vp) -> max acc (max_v t' (v+.vp))) 0. l
  in
  max_v t 0.

(*

Now we can rewrite our tree by recursively visiting it and changing
the vertical offset of the leaves that are not at the [bottom]
position. [rw] takes a subtree [t] and its current absolute vertical
position [av].  It returns a pair whose first element is the absolute
vertical position of the considered subtree if it is a single leaf and
[0.]  otherwise.  The second element is the rewritten subtree.  If the
recursive call doesn't return [0.] (case of a leaf) we adjust the
vertical offset to [bottom] minus the current leaf position; otherwise
we keep the same offset but use the returned subtree (it may contain
leaves whose offset has been adjusted).

*) 

let align_leaves t =
  let bottom = max_v t
  in
  let rec rw t av =
    match t with
    | Dleaf _ -> av,t
    | Dnode(bb, s, l) ->
	0., Dnode(bb, s, List.map 
		    (fun (n,hp,vp) -> 
		      let r,n' = rw n (av+.vp) in
		      if r=0. then (n',hp,vp) else (n,hp,bottom-.av))
		    l)
  in
  let _, t' = rw t 0. in t'


(*

The previous algorithm moves only leaves, so that subtrees are still
``flushed top''.  Another possibility is to move down as many nodes as
possible.  To do that, we compute for each node the offset that must
be applied on it.  The current node absolute vertical position is
[av], computed on prefix processing, the offset to be applied is
computed on postfix processing.  Each call returns both the computed
offset and the rewritten subtree.

For a leaf node, the offset is the [bottom] position minus its current
position as in the previous algorithm; the subtree obviously needs no
rewritting.  For an internal node we want to push it down as much as
possible, so we have to transfer to the node as much off the sons'
offsets as possible.  That is, the node gets the minimum offset of its
sons, the sons get this minimum subtracted from their offset.

We recurse and get the \emph{offset-tree list} for the sons [otl],
compute [min] from that and adjust the sons' offsets.

*)

let align_leaves2 t =
  let bottom = max_v t
  in
  let rec rw t av =
    match t with
    | Dleaf _ -> bottom-.av, t
    | Dnode(bb, s,l) ->
	let otl = List.map (fun (n,hp,vp) -> rw n (av+.vp)) l
	in
	let min = List.fold_left (fun acc (off,_) -> min acc off)
	    (fst (List.hd otl)) (List.tl otl)
	in
	min, Dnode(bb, s,
		   List.map (fun (n,hp,vp,off) -> (n,hp,vp+.off-.min))
		     (List.map2 (fun (n,hp,vp) (off,n') -> (n',hp,vp,off))
			l otl))
  in
  let _, t' = rw t 0. in t'

(*

This last alignment algorithm vertically ``stretches'' subtrees: when
a leaf node is moved down, all nodes on its path to the root are also
moved down to balance vertical space.

We start by associating with each node the length of the longest path
to a leaf in its subtree.  We could extend the [dtree] type to store
this information but we prefer store it in an array, with nodes
numbered in prefix order.  [size] counts the number of nodes to create
the array, [mark] does the computation.

inutile pour root ZZZ

*)

let rec size t =
  match t with
  | Dleaf _ -> 1
  | Dnode(_, _, l) ->
      List.fold_left (fun acc (n,_,_) -> acc + size n) 1 l

let mark t =
  let s = size t in
  let tab = Array.make s 0 in
  let cnt = ref (-1)
  in
  let rec mark t =
    incr cnt;
    let c = !cnt in
    let v = match t with
    | Dleaf _ -> 1
    | Dnode(_,_,l) -> 1 + List.fold_left max 0 (List.map (fun (n,_,_) -> mark n) l)
    in
    tab.(c) <- v; v
  in
  mark t; tab

(*

  As usual [av] is the current absolute vertical position, computed in
  prefix order. [myv] is the vertical offset set to the remaining
  vertical space ([bottom -. av]) divided by the number of nodes
  sharing this space.

  For an internal node, we recurse to get the list [vl] of vertical
  offsets of the sons and build our node so that each son gets its
  offset [itsv] from [vl].

  TODO: this is actually quite simple but the code is really unclear.

*)

let align_leaves3 t =
  let bottom = max_v t
  and tab = mark t
  and cnt = ref (-1)
  in
  let rec rw t av =
    incr cnt;
    let c = !cnt in
    let myv = if c=0 then 0. else (bottom -. av) /. (float_of_int tab.(c))
    in
    match t with
    | Dleaf _ -> myv, t
    | Dnode(bb, s, l) ->
	let vl = List.map (fun (n,_,_) -> rw n (av+.myv)) l
	in myv, Dnode(bb, s, List.map2 (fun (_,hp,_) (itsv,n) -> n, hp, itsv) l vl)
  in
  let _, t = rw t 0. in t


(*

\begin{figureh}
  \centering
  \sub{No leaf alignment}{align0}\hfill
  \sub{Moving leaves down}{align1}
  \sub{Moving trees down}{align2}
  \sub{Stretching trees}{align3}
  \caption{Vertical alignment policies}
  \label{fig:align}
\end{figureh}

*)




(*s An algorithm producing compact trees *)

(*

Sometimes we dont want our subtrees to be rigidly aligned in exclusive
areas.  The following algorithm allows subtrees spaces to overlap, as
long as the nodes themselves do not overlap.  This will make narrower
trees than the previous algorithm as shown on Figure
\ref{fig:compact}.

\begin{figureh}
   \centering\img{.6\linewidth}{compact}
   \caption{Compact version of our example}
   \label{fig:compact}
\end{figureh}

To do that, we'll define the shape of a tree to be its envelope,
defined as a series of float pairs, indicating left and right extent
at each level.  In the previous algorithm, we considered only the tree
width so its enveloppe was implicitly a simple rectangular box.  Now
we want each subtree to be positioned so that the left side of its
envelope is at [sep] distance of the right side of its left brother's.
\textbf{But} there are a few subtleties:

\begin{enumerate}
\item If [t] has a deeper envelope than it's left brother's [lb], we
  have to extend [lb]'s envelope with its own left brother's and so
  on.  Otherwise, an envelope may overlap with the one of a distant
  (non neighbor) brother.
\item Imagine a node has three subtrees A, B, and C, B being less deep
  than the two others.  We first place B next to A, then C next to the
  extension of B by A.  Now there are inequal space around B, since it
  is very close to A but C may not be very close to it.  To get a good
  looking tree, we have to move B to a balanced position.  This is
  illustrated in Figure \ref{fig:centerB}.
\end{enumerate}

\begin{figureh}
  \centering
  \img{6cm}{centerB}
  \caption{Subtree B should be centered between envelopes of A and C}
  \label{fig:centerB}
\end{figureh}

*)

(*s We first consider how to extend envelopes.*)

(* Take a prefix of l1 or l2 so they have the same length. *)

let cut l1 l2 =
  let prefix l n =
    let l,_ = List.fold_left
	(fun (l,n) e -> if n>0 then (e::l,n-1) else l,0)
	([],n)
	l
    in
    List.rev l
  in
  prefix l1 (List.length l2), prefix l2 (List.length l1)

  
(* Compute the max distance between enveloppes [e1] (on left) and [e2]
   (on right). *)

let delta e1 e2 =
  let e1, e2 = cut e1 e2
  in
  let r1l2 = List.map2 (fun (l1,r1) (l2,r2) -> r1,l2) e1 e2
  in
  List.fold_left (fun m (r1,l2) -> max m (r1-.l2)) 0. r1l2


(* Extend envelop [e] with previous one [ep], translated of [d]. *)

let extend ep d e =
  let rec ext ep e acc =
    match ep,e with
    | [], h2::t2 -> ext [] t2 (h2::acc)
    | (g1,d1)::t1, [] -> ext t1 [] ((g1+.d,d1+.d)::acc)
    | h1::t1, h2::t2 -> ext t1 t2 (h2::acc)
    | [], [] -> List.rev acc
  in
  ext ep e []


(* Give the min distance between [e1]'s right side and [e2]'s left
   side, their centers being separated by distance [delta]. *)

let dist_env e1 e2 delta =
  let e1, e2 = cut e1 e2
  in
  let lr = List.map2 (fun (l1,r1) (l2,r2) -> r1,l2) e1 e2
  in
  List.fold_left (fun m (l,r) -> min m (delta -. l +. r)) delta lr

(*s Balance brother nodes on their line. *)

(* 

So after having positioned all brother nodes we will have to check if
all are centered, and move them appropriately to the right if needed.
Unfortunately, moving one node to the right may render unbalanced
another previous node.  Moreover when moving B, we must consider the
extended envelope of A on its left, and the extended envelope of C on
its right.  Last, extended envelopes depend on node relative
positions, so they potentially need to be recomputed when a node is
moved.  We'll see below how we manage all this.

We'll perform moves iteratively.  At each iteration, we look for an
uncentered node.  If found, we compute its new position and start
immediately another iteration.  If not, then all nodes are well
positionned and we're done.  Centering means putting a node position
so that its enveloppe is at equal distance from its left and right
brothers \textbf{extended} envelopes.

\bigskip

Extend on the left.  That is, we're extending the \textbf{right} side
of [i]'s envelop with those of its left brothers, so that it is at
least as [deep]. *)

let extend_left envs pis i deep =
  let dp ni = pis.(ni) -. pis.(i)
  in
  let env = List.nth envs i
  in
  let rec help i myenv =
    if (List.length myenv) < deep && i>0 then
      help (i-1) (extend (List.nth envs (i-1)) (dp (i-1)) myenv)
    else myenv
  in
  help i env
;;

(* Extend on the right.  That is, we're extending the \textbf{left}
side of [i]'s envelop with those of its right brothers, so that ... *)

let extend_right envs pis i deep =
  let dp ni = pis.(ni) -. pis.(i)
  in
  let env = List.nth envs i in
  let n = List.length envs
  in
  let rec help i myenv =
    if (List.length myenv) < deep && i<n-1 then
      help (i+1) (extend (List.nth envs (i+1)) (dp (i+1)) myenv)
    else myenv
  in
  help i env


(*

Check centering of one node.  Our node has envelop [e], has an [el]
envelop at [dl] distance on its left, an [er] envelop at [dr] distance
on its right.  [el] and [er] are already extended if necessary, find
out of how much [e] should be moved to be centered.

 *)

let balance_one el e er dl dr =
  let ls = dist_env el e dl  (* left space *)
  and rs = dist_env e er dr  (* right space *)
  in
  let ns = (ls+.rs) /. 2.    (* new space *)
  in ns -. ls


(* Now, we can balance all nodes.  We use an exception to stop the
current iteration and proceed immediately to the next one when a node
has been moved for more than [min_move].  Rounding errors may cause
non termination if we insisted on really null move.  

*)

exception Node_moved
let min_move = 0.01

let balance pis envs =
   let n = List.length pis
   and pis = Array.of_list pis
   in
   let again = ref true
   in
   while !again do
     try
       again := false;
       for i=1 to n-2 do
	 let e = List.nth envs i in
	 let el = extend_left envs pis (i-1) (List.length e)
	 and er = extend_right envs pis (i+1) (List.length e)
	 in
	 let mv = balance_one el e er (pis.(i)-.pis.(i-1)) (pis.(i+1)-.pis.(i))
	 in
	 if (abs_float mv) > min_move then begin
	   pis.(i) <- pis.(i) +. mv; raise Node_moved
	 end
       done
     with Node_moved -> again := true
   done;
   pis.(0), Array.to_list pis, pis.(n-1)
;;

(*s Finally... *)

(*

Here's the second version of our algorithm, drawing compact trees.
Its structure is very similar with the first one.  The main difference
(apart from the complexities we saw in the computation of positions)
is the fact that shapes are now lists.

*)



let dtree_of_tree_compact v sep pos_root t =
  
  (* The shape of a leaf node: the position is the center of the node,
     by definition. ZZZ*)
  let leaf_shape n = let (l,r) = width n in [ l, r ]
  in

  (* The [cons]ed[_shape] is built from the root width and the merged
    shapes of the subtrees. *)

  let cons_shape n sh = 
    let (l,r) = width n in (l,r)::sh
  in

  let positions dsts shs =
    let pis, _, _ =
      List.fold_left
	(fun (pis, p, ei) e ->
	  let pi = p +. sep +. delta ei e in
	  pis @ [pi], pi, extend ei (p-.pi) e)
	([ 0. ], 0., List.hd shs)
	(List.tl shs)
    in
  
    (*c balance positions *)	

    let p1, pis, pn = balance pis shs
    in pis
      
  in

  let merge_shapes pis shs =
    let n = List.length pis in
    let p1 = List.nth pis 0
    and pn = List.nth pis (n-1)
    in
    let l1 = extend_right shs (Array.of_list pis) 0 99
    and rn = extend_left shs  (Array.of_list pis) (List.length shs - 1) 99
    in
    List.map2
      (fun (l,_) (_,r) -> l+.p1, r+.pn)
      l1 rn
  in
  
  let rec dt_of_t t =

    match t with
    | Leaf (bb,lb) -> Dleaf (bb,lb), leaf_shape t
	  
    | Node(bb,lb, subts) ->
	  (* first get the dtrees and shapes of all subtrees *)
	  let dsts, shs = List.split (List.map dt_of_t subts)
	  in
	  (* get relative subtrees positions *)
	  let pos = positions dsts shs in
	  (* shape of concatenated subtrees *)
	  let csh = merge_shapes pos shs in
	  (* position of root *)
	  let root = pos_root pos subts shs (List.hd csh) t
	  in
	  let pos = List.map (fun p -> p -. root) pos in
	  let csh = List.map (fun (l,r) -> l-.root, r-.root) csh
	  in
	  (* return global dtree and shape *)
	  Dnode(bb, lb, List.map2 (fun dst p -> dst,p,v) dsts pos),
	  (cons_shape t csh)
  in
  let dt,_  = dt_of_t t in dt



(*s Manual compaction *)

(*

To obtain more compaction, we allow the user to give a list of nodes
that have to be ``pushed down'' so that the tree can be narrower, as shown
on Figure \ref{fig:manual}.

\begin{figureh}
  \centering
   \subs{A tree with a large node}{.58}{manual0}\hfill
   \subs{With node 6 moved down}{.34}{manual6}
  \caption{Manual compaction}
  \label{fig:manual}
\end{figureh}


The basic idea is simple: we'll insert ``fake'' nodes (that must be of
zero width) in the tree at the given positions.  We will then generate
a [dtree] with our previous algorithm and finally remove the fake
nodes from it, pushing down the node just under by adding the
appropriate value to its vertical position.

Here [fake] is a special label value used to identify fake nodes.

*)

let insert_fakes fake ns t =
  let cnt = ref 0 in
  let ns = ref ((List.sort compare ns) @ [-1])
  in
  let rec ins t =
    match !ns with
    | n::nr ->
	incr cnt;
	let v = !cnt in
	if v=n then Node((0.,0.),fake, [ let _ = decr cnt; ns:=nr in ins t ])
      else
	  match t with
	  | Leaf _ -> t
	  | Node(bb,s,l) -> Node(bb, s, List.map ins l)
  in
  ins t

let remove_fake_nodes fake t =
  let rec rem t =
    match t with
    | Dnode(_, s, [ (n,hp,vp) ]) when s = fake -> 
	let vp', n' = rem n in vp+.vp', n'
    | Dleaf _ -> 0., t
    | Dnode(bb, s, l) -> 
	let l' = List.map (fun (n,hp,vp) -> let add, n' = rem n in (n',hp,vp+.add)) l
	in
	0., Dnode(bb, s, l')
  in
  let _, t = rem t in t


let dtree_of_tree_compact_nodes fake ns v sep pos_root t =

  let t = insert_fakes fake ns t in
  let dt = dtree_of_tree_compact v sep pos_root t
  in remove_fake_nodes fake dt



(*s Automatic compaction *)


(*

  Now we want the program to find out itself a (multi) set of nodes to
  be pushed down so as to narrow the tree.  We first need to be able
  to compute the width of a [dtree].  This is performed by
  [tree_width].  Note that the [width] function it is passed is the
  one we already saw, giving the width of a node from a [tree] (not a
  [dtree]).
*)

let tree_width t =
  let width dt =
    match dt with
    | Dleaf (bb,l) -> width (Leaf (bb,l))
    | Dnode(bb,l,_) -> width (Node(bb,l,[]))
  in
  let rec tw t p l r =
    let (lt,rt) = width t in
    let l = min l (p +. lt) and r = max r (p +. rt) in
    match t with
    | Dleaf _ -> l,r

    | Dnode(_ ,_, ns) -> 
	let lrs = List.map (fun (n,hp,vp) -> tw n (p+.hp) l r) ns
	in
	List.fold_left
	  (fun (ml,mr) (l,r) -> (min ml l, max mr r))
	  (l,r) lrs
  in
  tw t 0. 0. 0.

(*

  [size] computes the number of nodes of a tree, while [height]
  computes its height in nodes (since pushing down nodes to reduce
  width may also increase height).  Since we'll have to compute both
  width and height for all selected dtrees, the
  [dtree_of_tree_compact_nodes_height] function performs as
  [dtree_of_tree_compact_nodes] previously seen but also computes
  height.

*)

let rec size t =
  match t with
  | Leaf _ -> 1
  | Node(_, _, l) ->
      List.fold_left (fun acc n -> acc + size n) 1 l

let rec height t =
  match t with
  | Leaf _ -> 1
  | Node(_, _, l) ->
      1 + (List.fold_left (fun acc n -> max acc (height n)) 1 l)


let dtree_of_tree_compact_nodes_height fake ns v sep pos_root t =

  let t = insert_fakes fake ns t in
  let dt = dtree_of_tree_compact v sep pos_root t
  in height t, remove_fake_nodes fake dt


(*

A first approach is to perform a brute force search on msets of nodes.
The following function does that for msets of size two (more than two
usally means unacceptable run time).  Moving down only zero or one
node is also considered since node 1 (root) is part of the tried
nodes.

The function returns the original width and height along with the list
of the combinations that are better than [pct] percent (together with
their width and height).

*)


(*i ZZZ allow manual add ons -> orig width is what ? i*)

let brute_force_compact pct fake v sep pos_root t =
  let n = size t in
  let oh = height t
  in
  let dt = dtree_of_tree_compact v sep pos_root t in
  let l,r = tree_width dt in
  let ow = r-.l in
  let bound = ow *. ((float_of_int (100-pct)) /. 100.)
  in
  let res = ref []
  in
  for i = 1 to n do
    for j = i to n do
      let h, dt' = dtree_of_tree_compact_nodes_height fake [i;j] v sep pos_root t
	in
	let l',r' = tree_width dt' in
	if r'-.l' < bound then
	  res := (i,j,r'-.l',h) :: !res
    done
  done;
  (ow, oh, (List.sort (fun (_,_,w1,h1) (_,_,w2,h2) -> compare (w1,h1) (w2,h2)) !res))



(*s A smarter method for automatic compaction *)

(*

   Now a smarter method.  Our idea is to find the list of all nodes
   that, taken individually, reduce the tree width by a least a given
   percentage, and to iter this to build msets of nodes.

   [search_one_mode_node] builds a dtree with nodes from [ms] pushed
   down, and gets its width.  It then computes the widths of the
   dtrees obtained by pushing one more node, for all possible nodes
   and saves the added nodes for which the tree width is indeed
   sufficiently reduced.  It returns the list of msets for the
   ``winning'' nodes. (this list is possibly empty).

   We implement msets as sorted lists, to ensure an efficient equality
   test (we'll have to avoid building the same mset multiple times).

 *)


let rec insert n l =
  match l with
  | [] -> [ n ]
  | h::t -> if n<h then n::l else h::(insert n t)
    
let search_one_more_node ms pct fake v sep pos_root t =
  let n = size t
  in
  let ms' = List.map (fun (n,_) -> n) ms in
  let dt = dtree_of_tree_compact_nodes fake ms' v sep pos_root t in
  let l,r = tree_width dt in
  let bound = (r -. l) *. ((float_of_int (100-pct)) /. 100.)
  in
  let nodes = ref []
  in

  (* build list of winning nodes *)
  for i = 2 to n do
    let h, dt = dtree_of_tree_compact_nodes_height fake (i::ms') v sep pos_root t
    in
    let l',r' = tree_width dt in
    if r'-.l' < bound then begin
      nodes := (i, (r'-.l',h)) :: !nodes
    end
  done;
  (* return list of updated msets *)
  List.map (fun nn -> insert nn ms) !nodes


(*

  ZZZ  [add_mset] adds an mset to a list of msets

  TODO: decrease pct with each iteration ? 

*)


let add_mset n list =
  let rec mset_equal m1 m2 = 
    match m1, m2 with
    | (n1,_)::t1, (n2,_)::t2 when n1=n2 -> mset_equal t1 t2
    | [], [] -> true
    | _, _ -> false
  in
  if List.exists (fun e -> mset_equal e n) list
  then list else n :: list

let add_msets ns list =
  List.fold_left (fun list n -> add_mset n list) list ns

(*

  The [smart_compact] function returns the original width and height
  along with the list of the msets that are better than [pct] percent
  (together with their width and height).

*)

let smart_compact pct fake v sep pos_root t =

  let rec loop fin msets =
    let fin, cont =
      List.fold_left
	(fun (fin,cont) mset -> 
	  let nmsets = search_one_more_node mset pct fake v sep pos_root t
	  in
(*i	  List.iter
	    (fun ns -> Printf.printf "[ %s ]\n" 
		(String.concat "," (List.map (fun (i,_)->string_of_int i) ns)))
	    nmsets;
	  print_string "\n"; i*)
	  if List.length nmsets > 0
	  then add_mset mset fin, add_msets nmsets cont
	  else add_mset mset fin, cont)
(*i	  if List.length nmsets > 0 then (fin, nmsets @ cont) else mset::fin,cont) i*)

	(fin,[])
	msets
    in
    if cont = [] then fin
    else
      loop fin cont
  
  in
  let nodes = loop [] [[]]
  in
  let dt = dtree_of_tree_compact v sep pos_root t in
  let l,r = tree_width dt in
  let ow = r -. l in
  let oh = height t
  in
  (* build the list of (mset, width, height) *)
  let res = List.map
      (fun nodes ->
	let is, whs = List.split nodes in
	let ws, hs  = List.split whs in
	let w = List.fold_left min ow ws in
	let h = List.fold_left max oh hs
	in
	is, w, h)
      (List.rev nodes)
  in
  let cmp (i1, w1, h1) (i2, w2, h2) = compare (w1,h1) (w2,h2)
  in
  ow, oh, List.sort cmp res


(*s Possible extensions *)

(*

In our algorithms, labels are always horizontally centered around the
node position (where the edge from the father lies).  It would be nice
to be able to select the centering policy of node labels too,
especially when using the \emph{flush on node edges} policy.

We could embed this policy in the [width] function that would return a
couple of floats indicating the left and right length of the node.
Each node in the [dtree] would then store this information for the
drawing routines to use it.

Other nice extensions we can think of include considering variable
node height, and enhancements to our ``smart'' automatic compaction
algorithm.  A simple one would be to allow the user to specify
initially pushed down nodes (since it won't find a combination of two
nodes that is very good if both nodes are individually bad).

*)

(*s Illustration *)

(*

The following figure shows a nice compact representation of a
derivation tree produced by a syntax analysis tool.

\begin{figureh}
  \centering\imgl{25cm}{compact2}
%  \caption{A larger example of a compact tree}
  \label{fig:compact2}
\end{figureh}
*)
