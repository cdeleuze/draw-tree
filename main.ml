
open Tree

(* graphics parameters *)

let fill = ref 255 (* grey level of label box *)
let v  = ref 30    (* vertical distance between center of nodes *)
let va = ref 0     (* length of edge vertical part above node *)
let vb = ref 0     (* length of edge vertical part below node *)

(* [charwidth] and [charheight] are set to 6. and 10 as default value,
   the ones needed for eps generation.  They will be changed for
   screen display, according to default font.  The screen width of a
   reference string will be put in [refwidth] to compute actual char
   width and height to be used on current screen.  See function
   [compute_charsize] below *)

let charwidth = ref 6. 
let charheight = ref 10
let refwidth = ref 0.

(*s Making pictures *)

let sons n =
  match n with
  | Dleaf _ -> []
  | Dnode(_, _, s) -> s
;;

let draw_tree draw_node draw_edge node x y =

  let process_sons call x y ss =
    List.flatten (List.map (fun (n,hp,vp) -> call x y (x+.hp,y+.vp,n)) ss)
  in
  
  let rec help cf lf (c,l,n) =
    (process_sons help c l (sons n)) @
    (draw_edge cf lf c l) @ (draw_node c l n)
  in
  
  match node with Dnode(_, t,s) ->
    (process_sons help x y s) @ (draw_node x y node)
  | Dleaf(_,l) -> draw_node 0. 0. node
;;


let x_ x = int_of_float (!charwidth *. x);;   (* x unit is char, convert to pixel *)
let y_ y = int_of_float (-. y);;

open Pictures

let draw_node x y n = 
  let t = match n with Dleaf (_,t) -> t | Dnode(_, t,_) -> t
  in
  let dx = x_( (1.+. (float_of_int (String.length t)))/.2.)
  in
  (* is this a good idea? the bbox changes slightly... *)
  if !fill = 255 then [ Set_rgb_color (0,0,0); CText(x_ x,y_ y,t) ] else
  (* the -4 in y is what looks better when generating eps *)
  [  Set_rgb_color (!fill,!fill,!fill); Fill_rect(x_ x-dx, y_ y-4, dx*2, !charheight+4);
     Set_rgb_color (0,0,0); CText(x_ x,y_ y,t) ]

let draw_edge x1 y1 x2 y2 =
  let x'1 = x_ x1 and y'1 = y_ y1
  and x'2 = x_ x2 and y'2 = y_ y2
  in
  [ Segment(x'1, y'1 - 4,        x'1, y'1 - 4 - !vb);
    Segment(x'1, y'1 - 4 - !vb,  x'2, y'1 - !v + !va + !charheight);
    Segment(x'2, y'1 - !v + !va + !charheight,  x'2, y'2 + !charheight) ]
    
let no_draw_edge x1 y1 x2 y2 = []


let pict_of_tree tree =
  make_picture (draw_tree draw_node draw_edge tree 0. 0.)

let pict_of_tree_ne tree =
  make_picture (draw_tree draw_node no_draw_edge tree 0. 0.)

(* TODO: the +4 and -19 here are not extremely obvious *)

let bottom_up pict =
  match pict with { cmds=c; x=x; y=y; width=w; height=h }
    ->
      let bc = List.map
      (fun e -> match e with
      | Fill_rect(x1,y1,w,h) -> Fill_rect(x1,-y1-h,w,h)
      | CText(x,y,t) -> CText(x,-y- !charheight+4,t)
      | Segment(x1,y1,x2,y2) -> Segment(x1,-y1,x2,-y2)
      | _ -> e)
      c in
      { cmds=bc; x=x; y=y+h-19; width=w; height=h }


(*s Main function *)


let output_eps = ref ""
let output_fig = ref ""
let sep    = ref 3.
let width  = ref (-1.)
let left  = ref false
let right = ref false
let edge  = ref false
let env   = ref false
let noedge = ref false

let compact = ref false
let bf = ref (-1)
let cn = ref []
let align = ref 0
let bup = ref false

let rec rw_tree_width l r t =
  match t with
  | Leaf (_,t) -> Leaf((l,r),t)
  | Node(_,t,o) -> Node ((l,r),t,List.map (rw_tree_width l r) o)

let search = ref (-1)

(* return output file name, either the one provided
   or one constructed from the tree file name if output is "-"
 *)

let make_output_name out name ext =
  if out = "-" then
    let m =
      try Filename.chop_extension name
      with Invalid_argument _ -> name
    in
    m ^ "." ^ ext
  else out

let debug s = print_string (s ^ "\n"); flush stdout

(* This sets the values of refwidth and charwidth for scaling
   graphics display.  These values depend on the default font, and in
   particular are not the sames on X11 and Windows. *)

let compute_charsize () =
  let fdiv a b = float_of_int a /. float_of_int b
  in
  Graphics.open_graph ":0";
  let text = "ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFG" in
  let (w,h) = Graphics.text_size text in 
  refwidth := float_of_int w;
  charheight := h;
  charwidth := !refwidth /. (float_of_int (String.length text));
  debug (Printf.sprintf "refwidth=%.2f charwidth=%.2f charheight=%i\n"
	   !refwidth !charwidth !charheight)

let display_tree dt =
  (* The picture has been built for the 'fixed' font.  For scaling the
     picture on screen we will use the following font, available in
     several sizes.  When changing the font size, we need to scale the
     picture dimensions accordingly.  We use the length of a reference
     string (see file font.ml) to determine the scale factor. [sizes]
     contains the available sizes for the selected font along with the
     length of the reference string.

     The reference string has length !refwidth for 'fixed' font, so the
     scale factor is the reference string length for the selected font
     divided by !refwidth.
  *)
  compute_charsize ();
  let sizes = if Sys.os_type = "Win32" 
    then (* Scaling not currently supported on Win32 display *)
      let (w,r) = (int_of_float !charwidth,!refwidth) in [| w,r;w,r;w,r |]
    else [| 6,132.; 8,165.; 10,198.; 14,231.; 18,297.; 20,330. |] 
  in
  let make_p dt = (if !bup then bottom_up else (fun p -> p))
    ((if !noedge then pict_of_tree_ne else pict_of_tree) dt)
  in
  let x,y,w,h = bb (make_p dt) in
  Graphics.open_graph (":0 " ^ string_of_int w ^ "x" ^ string_of_int h);
  Graphics.set_window_title "tree viewer";

  let font  = "-misc-fixed-medium-r-normal--%i-*" 
  in

  (* use a zipper-like data structure to focus on a part on the tree *)
  let focus (z,dt) k =
    let sons = match dt with
      | Dleaf _ -> None
      | Dnode(bb,label,sons) -> Some sons
    in
    match k with
      'K' -> (match z with
	(n,h)::t -> t,h
      | []       -> ([], dt))

    | 'J' -> (match sons with
      | None -> (z,dt)
      | Some sons -> let n = (List.length sons-1) /2 in
		     ((n,dt)::z), let (s,_,_) = List.nth sons n in s)

    | 'H' | 'L' -> (match z with
      (n,Dnode(bb,label,sons))::t -> 
	let n'= if k='H' then n-1 else n+1 in
	if n'<List.length sons && n'>=0 then
	  let (s,_,_) = List.nth sons n'
	  in ((n',Dnode(bb,label,sons))::t,s)
	else
	  (n,Dnode(bb,label,sons))::t,dt
      | z -> z,dt)
  in

  let rec loop xw yw i ((zip, dt) as d) =
    let size,width = sizes.(i) in let sc = width /. !refwidth in
    let x,y,w,h = bb (make_p dt) in
    let ww = int_of_float ((float_of_int w) *. sc) in
    let wh = int_of_float ((float_of_int h) *. sc) in

    (* Warning: we need to [resize_window] *before* [set_font]! *)
    Graphics.resize_window ww wh;
    Graphics.clear_graph (); (* because resize_window does not work on ... Windows *)
    Graphics.set_font (Printf.sprintf (Obj.magic font) size);
    Printf.printf "isz=%i size=%i width=%.0f ww=%i" i size width ww; print_newline();

    to_screen (make_p dt) xw yw ww wh;
    let k = Graphics.read_key () in
    match k with 
      'l' -> loop (xw - 100) yw i d
    | 'h' -> loop (xw + 100) yw i d
    | 'j' -> loop xw (yw + 100) i d
    | 'k' -> loop xw (yw - 100) i d
    | '<' -> loop 0 yw i d
      (* size_x/y may give a value larger than actual screen...*)
    | '>' -> loop (Graphics.size_x()-ww) yw i d
    | '^' -> loop xw (Graphics.size_y()-wh) i d
    | 'v' -> loop xw 0 i d


    | 'q' -> ()
    | '+' -> loop xw yw (if i < Array.length sizes - 1 then i+1 else i) d
    | '-' -> loop xw yw (if i > 0 then i-1 else i) d
    | 'J' | 'K' | 'H' | 'L' -> loop xw yw i (focus (zip,dt) k)
    |  _  -> loop xw yw i d
  in
  loop 0 0 2 ([], dt)
    

(* Read tree, select algorithm and various parameters, produce
   output. *)

let main file sep =
  let string_args = String.concat " " (Array.to_list Sys.argv)
  in
  let t = Text.read_file file in
  let t = 
    if !width = -1. then t else
    rw_tree_width (-. !width /. 2.) (!width /. 2.) t
  in
  let root_pos = 
    if !env then root_centers else
    if !left then if !edge then root_leftn else root_leftp else
    if !right then if !edge then root_rightn else root_rightp
    else if !edge then root_centern else root_centerp
  in
  let draw = 
    if !compact then
      dtree_of_tree_compact (float_of_int !v) sep root_pos      
    else if !cn=[] then
      dtree_of_tree (float_of_int !v) sep root_pos
    else
      dtree_of_tree_compact_nodes "" !cn (float_of_int !v) sep root_pos
  in
  let draw = if !align=1 then fun t -> align_leaves  (draw t) else
             if !align=2 then fun t -> align_leaves2 (draw t) else
             if !align=3 then fun t -> align_leaves3 (draw t) else
	     draw
  in
  if !search <> -1 then begin
    let ow, oh, res = smart_compact !search "" (float_of_int !v) sep root_pos t
    in
    let l = List.length res in
    Printf.printf "original width %.2f\n" ow;
    Printf.printf "%i msets\n" l;
    List.iter 
      (fun (is, w, h) ->
	Printf.printf "width=%.2f (%2.0f %%) height=%i (%+i) [ %s ]\n"
	  w (w*.100./.ow) h (h-oh)
	  (String.concat ", "
	     (List.map string_of_int is)))
      res;
    exit 0
  end;
  if !bf <> -1 then begin
    let ow, oh, l = brute_force_compact !bf "" (float_of_int !v) sep root_pos t in
    Printf.printf "original width %.2f\n" ow;
    List.iter
      (fun (i,j,w,h) -> Printf.printf "[%i,%i] %.2f (%2.0f %%) height=%i (+%i)\n"
	  i j w (w*.100./.ow) oh (h-oh))
      l;
    exit 0
  end;

  let dt = draw t in

  let p = (if !bup then bottom_up else (fun p -> p))
    ((if !noedge then pict_of_tree_ne else pict_of_tree) dt)
  in
 
  let l,r = tree_width dt in
  Printf.printf "width=%.2f" (r-.l);
  print_newline ();

  if !output_eps <> "" then
    Pictures.to_eps_file 
      (make_output_name !output_eps file "eps")
      ("Command line: " ^ string_args) p
  else
  if !output_fig <> "" then
    Pictures.to_fig_file
      (make_output_name !output_fig file "fig")
      ("Command line: " ^ string_args) p
  else
    display_tree dt
;;

(*s Parsing of arguments *)

Arg.parse 
[
 "-oe", Arg.Set_string output_eps, "<file> output to eps file";
 "-of", Arg.Set_string output_fig, "<file> output to fig file";
 "-sep", Arg.Set_float  sep,    "<sep>  node separation (default "
 ^ string_of_float !sep ^ ")";
 "-w", Arg.Set_float  width,  "<width> node width (default is label length)";
 "-l", Arg.Set  left,  "flush left";
 "-r", Arg.Set  right,  "flush right";
 "-e", Arg.Set  edge,  "align on edge (not positions)";
 "-s", Arg.Set  env,   "center on shape";
 "-a", Arg.Set_int align, "<n> align leaves (0 none, 1 move leaves down, 2 move subtrees down, 3 center subtrees, default " ^ string_of_int !align ^ ")";
 "-u", Arg.Set  bup, "draw bottom-up (Upside-down)";
 "-c", Arg.Set  compact, "use compact algorithm";
 "-n", Arg.Set  noedge, "dont draw edges";
 "-b", Arg.Set_int fill, "grey level for label boxes (0=black, 255=white, default "
                         ^ string_of_int !fill ^")";
 "-v", Arg.Set_int v, "vertical space between nodes (default " ^ string_of_int !v ^ ")";
 "-va", Arg.Set_int va, "edge length above node (default " ^ string_of_int !va ^ ")";
 "-vb", Arg.Set_int vb, "edge length below node (default " ^ string_of_int !vb ^ ")";
 "-d", Arg.Int (fun i -> cn := i :: !cn), "<n> move down node n (can be repeated)";
 "--bf-compact", Arg.Set_int  bf, "<p> brute force compaction: allow moving 2 nodes down to compact, display all combinations that reduce width by at least p percent.";
 "--smart-compact", Arg.Set_int search, "<p> smart compaction: try moving down nodes as long as this provides a gain of at least p percent in width."
]
  (fun s -> main s !sep) "tree <options> <input_file>\n\
by default display on screen.  Move with vi-like bindings, scale with -/+, navigate with uppercase vi-like bindings."
