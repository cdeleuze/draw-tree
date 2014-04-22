
open Tree

(* graphics parameters *)

let fill = ref 255 (* grey level of label box *)
let v  = ref 30    (* vertical distance between center of nodes *)
let vu = ref 4     (* size of up vertical edge ZZZ 4 = 0 ? *)
let vb = ref 12    (* size of bottom vertical edge *)

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
;;


let x_ x = int_of_float (6. *. x);;   (* largeur d'un caractère *)
let y_ y = int_of_float (-. y);;

open Pictures

let draw_node x y n = 
  let t = match n with Dleaf (_,t) -> t | Dnode(_, t,_) -> t
  in
  let dx = x_( (1.+. (float_of_int (String.length t)))/.2.)
  in
  [  Set_rgb_color (!fill,!fill,!fill); Fill_rect(x_ x-dx, y_ y-4, dx*2, 16);
     Set_rgb_color (0,0,0); CText(x_ x,y_ y,t) ]

let draw_edge x1 y1 x2 y2 =
  let x'1 = x_ x1 and y'1 = y_ y1
  and x'2 = x_ x2 and y'2 = y_ y2
  in
  [ Segment(x'1, y'1,        x'1, y'1 - !vu);
    Segment(x'1, y'1 - !vu,  x'2, y'1 - !v + !vb);
    Segment(x'2, y'1 - !v + !vb,  x'2, y'2) ]
    
let no_draw_edge x1 y1 x2 y2 = []


let pict_of_tree tree =
  make_picture (draw_tree draw_node draw_edge tree 0. 0.)

let pict_of_tree_ne tree =
  make_picture (draw_tree draw_node no_draw_edge tree 0. 0.)

(* TODO: rewrite that, it's awfull !+ check vu vb *)

let bottom_up pict =
  match pict with { cmds=c; x=x; y=y; width=w; height=h }
    ->
      let bc = List.map
      (fun e -> match e with
      | Fill_rect(x1,y1,w,h) -> Fill_rect(x1,-y1-h,w,h)
      | CText(x,y,t) -> CText(x,-y-9,t)
      | Segment(x1,y1,x2,y2) -> Segment(x1,-y1,x2,-y2)
      | _ -> e)
      c in
      { cmds=bc; x=x; y=y+h-24; width=w; height=h }


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

let display_tree p =
  (* The picture has been built for the 'fixed' font.  For scaling the
     picture on screen we will use the following font, available in
     several sizes.  When changing the font size, we need to scale the
     picture dimensions accordingly.  We use the length of a reference
     string (see file font.ml) to determine the scale factor. [sizes]
     contains the available sizes for the selected font along with the
     length of the reference string.

     The reference string has length 198 for 'fixed' font, so the
     scale factor is the reference string length for the selected font
     divided by 198.
  *)
  let font  = "-misc-fixed-medium-r-normal--%i-*" in
  let sizes = [| 6,132.; 8,165.; 10,198.; 14,231.; 18,297.; 20,330. |] 
  in
  let x,y,w,h = bb p in
  Graphics.open_graph (":0 " ^ string_of_int w ^ "x" ^ string_of_int h);

  let cont = ref true in 
  let xw = ref 0 and yw = ref 0 in
  let i = ref 2 
  in
  while !cont do
    let size,width = sizes.(!i) in let sc = width /. 198. in
    let ww = int_of_float ((float_of_int w) *. sc) in
    let wh = int_of_float ((float_of_int h) *. sc) in

    (* Warning: we need to [resize_window] *before* [set_font]! *)
    Graphics.resize_window ww wh;
    Graphics.set_font (Printf.sprintf (Obj.magic font) size);
    Printf.printf "isz=%i size=%i width=%f ww=%i" !i size width ww; print_newline();

    to_screen p !xw !yw ww wh;
    let k = Graphics.read_key () in
    match k with 
      'l' -> xw := !xw - 100
    | 'h' -> xw := !xw + 100
    | 'j' -> yw := !yw + 100
    | 'k' -> yw := !yw - 100
    | '<' -> xw := 0
    | '>' -> xw := -w / 2 (* we should set to w minus the screen size, but we don't know it *)
    | 'q' -> cont := false
    | '+' -> if !i < Array.length sizes - 1 then incr i
    | '-' -> if !i > 0 then decr i
    |  _  -> ()
  done


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
  let dt = draw t in

  let p = (if !bup then bottom_up else (fun p -> p))
    ((if !noedge then pict_of_tree_ne else pict_of_tree) dt)
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
    display_tree p
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
 "-u", Arg.Set  bup, "draw bottom-up";
 "-c", Arg.Set  compact, "use compact algorithm";
 "-n", Arg.Set  noedge, "dont draw edges";
 "-b", Arg.Set_int fill, "grey level for label boxes (0=black, 255=white)";
 "-v", Arg.Set_int v, "vertical space (default " ^ string_of_int !v ^ ")";
 "-vu", Arg.Set_int vu, "up vertical space (default " ^ string_of_int !vu ^ ")";
 "-vb", Arg.Set_int vb, "bottom vertical space (default " ^  string_of_int !vb ^ ")";
 "-d", Arg.Int (fun i -> cn := i :: !cn), "<n> move down node n (can be repeated)";
 "--bf-compact", Arg.Set_int  bf, "<p> brute force compaction: allow moving 2 nodes down to compact, display all combinations that reduce width by at least p percent.";
 "--smart-compact", Arg.Set_int search, "<p> smart compaction: try moving down nodes as long as this provides a gain of at least p percent in width."
]
  (fun s -> main s !sep) "tree <options> <input_file>\n\
by default display on screen.  Move with vi-like bindings, scale with -/+."
