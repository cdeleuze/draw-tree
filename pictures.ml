(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2000 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id: graphics.ml,v 1.19 2003/09/30 16:14:10 weis Exp $ *)

open Printf;;

let graphps_version = "1.2";;
let picture_version = "0.13";;

exception Picture_failure of string;;

(* axes -> x ^ y (y going up) for pictures, postscript and graphics

   xfig has y going down
*)

(* text -- y is bottom line, for postcript, graphics and xfig *)
(* same for pictures ? *)

type command =
   | Open_pict of int * int * int * int
   | Close_pict

   | CText of int * int * string  (* horizontally centered -- vertically ? *)
   | Segment of int * int * int * int

   | Curveto of (int * int) * (int * int) * (int * int)

   | Draw_rect of int * int * int * int
   | Fill_rect of int * int * int * int

   | Draw_poly of (int * int) array
   | Fill_poly of (int * int) array

   | Draw_arc of int * int * int * int * int * int
   | Fill_arc of int * int * int * int * int * int

   | Draw_ellipse of int * int * int * int
   | Fill_ellipse of int * int * int * int

   | Draw_circle of int * int * int
   | Fill_circle of int * int * int

   | Set_line_width of int

   | Set_rgb_color of int * int * int
   | Plot of int * int
   | Moveto of int * int
   | Lineto of int * int
   | Rmoveto of int * int
   | Rlineto of int * int

(*i   | Draw_char of char
   | Draw_string of string
   | Set_font of string * int i*)
;;


type picture = {
    mutable cmds : command list;
    mutable x:int;
    mutable y:int;
    mutable width : int;
    mutable height : int
  } 




(* imperative style *)

let create_picture x0 y0 w h = { cmds = [ Open_pict(x0,y0,w,h) ];
				 x=x0; y=x0; width=w; height=h };;

let add_command pict c =

  let add c = pict.cmds <- c :: pict.cmds in
  let set_point x y = pict.x <- x; pict.y <- y in
  let rset_point dx dy = pict.x <- pict.x+dx; pict.y <- pict.y+dy
  in
  
  match c with
  | Moveto(x,y)
  | Lineto(x,y)    -> set_point x y; add c
  | Rlineto(dx,dy)
  | Rmoveto(dx,dy) -> rset_point dx dy; add c
  | Curveto(p1,p2,(x3,y3)) -> set_point x3 y3; add c

  | Open_pict _
  | Close_pict 
  | Plot _
  | Draw_arc _
  | Draw_ellipse _
  | Draw_circle _
  | Set_line_width _
  | Draw_rect _
  | Fill_rect _
  | Fill_poly _
  | Draw_poly _
  | Fill_arc _
  | Fill_ellipse _
  | Fill_circle _    -> add c

(*i
  | Draw_char c      -> rset_point !x_text_size 0; add c
  | Draw_string s    -> rset_point (String.length s *!x_text_size) 0; add c

  | Set_font _ -> add c (*ZZZ*)
i*)
   (* draw poly line ?/draw segments *)


let translate_cmd dx dy cmd =
  match cmd with
  | Open_pict (x0, y0, w, h) -> Open_pict(x0+dx,y0+dy,w,h)
  | Close_pict -> cmd
  | CText(x,y,s) -> CText(x+dx, y+dy, s)
  | Segment(x1,y1,x2,y2) -> Segment(x1+dx,y1+dy,x2+dx,y2+dy)
  | Set_rgb_color (r, g, b) -> cmd
  | Plot (x, y) -> Plot(x+dx,y+dy)
  | Moveto (x, y) -> Moveto(x+dx, y+dy)
  | Lineto (x, y) -> Lineto(x+dx, y+dy)
  | Rmoveto _ -> cmd
  | Rlineto _ -> cmd
  | Curveto ((x1, y1), (x2, y2), (x3, y3))
    -> Curveto ((x1+dx, y1+dy), (x2+dx, y2+dy), (x3+dx, y3+dy))
  | Set_line_width _ -> cmd
  | Draw_rect (x, y, w, h) -> Draw_rect(x+dx, y+dy, w, h)
  | Fill_rect (x, y, w, h) -> Fill_rect(x+dx, y+dy, w, h)

  | Draw_poly(tab) -> Draw_poly(Array.map (fun (x,y) -> (x+dx, y+dy)) tab)
  | Fill_circle(x,y,r) -> Fill_circle(x+dx, y+dy, r)
;;

let translate dx dy pict =
  let cmds = List.map (translate_cmd dx dy) pict.cmds
  in
  { cmds = cmds; x=pict.x+dx; y=pict.y+dy; width=pict.width; height=pict.height }
;;

let merge p1 p2 =
  let c1 = List.rev (List.tl p1.cmds)
  and c2 = List.rev (List.tl p2.cmds)
  in
  let h1 = List.hd c1
  and h2 = List.hd c2
  in
  let x, y, w, h =
  match h1, h2 with
  | Open_pict(x1,y1,w1,h1), Open_pict(x2,y2,w2,h2)
    -> let x = min x1 x2 and y = min y1 y2
    and xt = max (x1+w1) (x2+w2)
    and yt = max (y1+h1) (y2+h2)
    in 
    x, y, (xt-x), (yt-y)
  | _ -> raise (Picture_failure "merge: missing Open_pict")
  in
  let p = create_picture x y w h
  in p.cmds <- Close_pict :: (List.rev (List.tl c1) @ (List.tl c2)) @ p.cmds;
  p


(* works only if not translated! *)
let scale_cmd sx sy cmd =
  match cmd with
  | Open_pict (x0, y0, w, h) -> Open_pict(x0*sx,y0*sx,w*sx,h*sy)
  | Close_pict -> cmd
  | CText(x,y,s) -> CText(x*sx,y*sy,s)
  | Segment(x1,y1,x2,y2) -> Segment(x1*sx,y1*sy,x2*sx,y2*sy)
  | Set_rgb_color (r, g, b) -> cmd
  | Plot (x, y) -> Plot(x*sx,y*sy)
  | Moveto (x, y) -> Moveto(x*sx, y*sy)
  | Lineto (x, y) -> Lineto(x*sx, y*sy)
  | Rmoveto (x, y) -> Rmoveto(x*sx, y*sy)
  | Rlineto (x, y) -> Rlineto(x*sx, y*sy)
  | Curveto ((x1, y1), (x2, y2), (x3, y3))
    -> Curveto ((x1*sx, y1*sy), (x2*sx, y2*sy), (x3*sx, y3*sy))
  | Set_line_width w -> cmd
  | Draw_rect (x, y, w, h) -> Draw_rect(x*sx, y*sy, w*sx, h*sy)
  | Fill_rect (x, y, w, h) -> Fill_rect(x*sx, y*sy, w*sx, h*sy)

  | _ -> failwith "scale_cmd: unknown cmd";;


let scale sx sy pict =
  let cmds = List.map (scale_cmd sx sy) pict.cmds
  in
  { cmds = cmds; x=0; y=0; width=0; height=0 }
;;
  
let bb_cmd cmd = 
  match cmd with
  | Open_pict _
  | Close_pict 
  | Set_rgb_color _
  | Set_line_width _ 
    -> None

  | CText(x,y,s) -> let l = 6*String.length s
	in Some(x-l/2,y,x+l/2, y+6) (* ZZZ *)

  | Segment(x1,y1,x2,y2) -> Some(x1,y1,x2,y2)

  | Draw_rect (x, y, w, h) 
  | Fill_rect (x, y, w, h) -> Some(x,y,x+w,y+h)

  | Plot (x, y) ->   Some(x,y,x,y)
  | Moveto (x, y) -> Some(x,y,x,y)
  | Lineto (x, y) -> Some(x,y,x,y)

  | Draw_circle(x,y,r)
  | Fill_circle(x,y,r) -> let r = abs r in Some(x-r,y-r,x+r,y+r)

  | Rmoveto (x, y) -> failwith "bb_cmd: rmoveto"
  | Rlineto (x, y) -> failwith "bb_cmd: rlineto"

  | Curveto ((x1, y1), (x2, y2), (x3, y3)) -> failwith "bb_cmd: curveto"

  | _ -> failwith "bb_cmd: unknown cmd";;


(* compute and set bounding box of a picture *)

let set_bb p = 
  match p with { cmds=c; x=x; y=y; width=w; height=h }
    ->
      let fct  (xl,yb,xr,yt) cmd =
	match bb_cmd cmd with
	| None -> (xl,yb,xr,yt)
	| Some(x1,y1,x2,y2) -> ((min x1 xl), (min y1 yb), (max x2 xr), (max y2 yt))
      in
      let x1,y1,x2,y2 = List.fold_left fct
	  (max_int,max_int,min_int,min_int) c
      in
      { cmds=c; x=x1; y=y1; width=x2-x1; height=y2-y1 }
;; 

(* get current bounding box of a picture *)
let bb p = match p with
  { cmds=_; x=x; y=y; width=w; height=h }
  -> x,y,w,h



(* functional style *)

let make_picture cmds =
  set_bb
    {
     cmds = List.rev (cmds @ [ Close_pict ]);
     x=0; y=0; width=0; height=0 
   }
;;


open Graphics


(* -------------- screen export ----------------- *)

let screen_of_cmd dx dy sc cmd =
  let x_ x = (int_of_float ((float x) *. sc)) + dx
  and y_ y = (int_of_float ((float y) *. sc)) + dy
  and w_ w = (int_of_float ((float w) *. sc))
  in
  match cmd with
  | Open_pict(x,y,w,h) -> () (*i set_color background; fill_rect (x_ x) (y_ y) (w_ w) (w_ h); set_color foreground i*)

  | Draw_rect(xb,yb,w,h) -> draw_rect (x_ xb) (y_ yb) (w_ w) (w_ h)
  | Fill_rect(xb,yb,w,h) -> fill_rect (x_ xb) (y_ yb) (w_ w) (w_ h)
  | Draw_circle(xo,yo,r) -> draw_circle (x_ xo) (y_ yo) (w_ r)
  | Fill_circle(xo,yo,r) -> fill_circle (x_ xo) (y_ yo) (w_ r)
  | Draw_poly(t) -> draw_poly_line (Array.map (fun (x,y) -> (x_ x, y_ y)) t) 

  | CText(x,y,s) -> let tx,ty = text_size s in
    moveto ((x_ x)-tx/2) ((y_ y)); draw_string s

  | Segment(x1,y1,x2,y2) -> moveto (x_ x1) (y_ y1); lineto (x_ x2) (y_ y2)

  | Moveto(x,y) -> moveto (x_ x) (y_ y)
  | Lineto(x,y) -> lineto (x_ x) (y_ y)
  | Rlineto(x,y) -> rlineto (w_ x) (w_ y)
  | Rmoveto(x,y) -> rmoveto (w_ x) (w_ y)
  | Curveto((xa,ya),(xb,yb),(xc,yc)) ->
      curveto (x_ xa,y_ ya) (x_ xb,y_ yb) (x_ xc,y_ yc)
  | Set_rgb_color(r,g,b) -> set_color (rgb r g b)
  | _ -> ()

(* display pict on screen on window (xw,yw,ww,hw) *)

let to_screen pict xw yw ww hw =
  let x,y,w,h = bb pict in
  (* compute scale *)
  let sc = min ((float hw)/.(float h)) ((float ww)/.(float w))
  in
  (* compute translation parameters *)
  let dx = xw - (int_of_float ((float x)*.sc))
  and dy = yw - (int_of_float ((float y)*.sc))
  in
  (* draw to screen *)
  List.iter (screen_of_cmd dx dy sc) (List.rev pict.cmds)

let to_sc pict xw yw =
  let x,y,w,h = bb pict in
  to_screen pict xw yw w h
;;

(* ------------ postscript export ------------ *)


let eps_mode = ref true;;
let fonts : (string, string) Hashtbl.t = Hashtbl.create 11;;
let default_font = "Courier";; (*"Helvetica-Bold";;*)
Hashtbl.add fonts default_font default_font;;
let default_font_size = 10;;


(* Post Script functions embedded into the output *)
let ps_defs = "\
/ctext { dup stringwidth pop 2 div neg 0 rmoveto show } def
/m { moveto } def
/rm { rmoveto } def
/l { lineto } def
/c { currentpoint } def
/cp { currentlinewidth currentpoint } def
/ms { c stroke m } def
/dl { l ms } def
/rl { rlineto ms } def
/p { newpath m c lineto 1 setlinewidth stroke m setlinewidth } def
/pr  %% define the path for a rectangle w h x y
    { newpath m
      dup %% w h h
      0 exch rlineto %% rlineto 0 h -- w h
      exch 0 rlineto %% rlineto w 0 -- h
      0 exch sub 0 exch rlineto %% rlineto 0 -h --
      closepath } def
/dr %% w h x y
    { pr stroke m } def
/fr { pr fill m } def
/fc { 0 360 newpath arc fill m } def
/dc { 0 360 newpath arc stroke m } def
/fa { gsave translate scale newpath 0 0 m arc closepath fill grestore } def
/da { savematrix gsave translate scale newpath arc
      restorematrix stroke grestore } def
/fe { gsave translate scale newpath 0 0 1 0 360 arc fill grestore } def
/de { savematrix gsave translate scale newpath 0 0 1 0 360 arc
      restorematrix stroke grestore } def
/pc { newpath m } def
/dp { closepath stroke m } def
/fp { closepath fill m } def
/ct { curveto c stroke m } def
/kcolor { 1 255 div } def
/color { kcolor mul } def
/stmp { /tmp exch def } def
/srgb {% r g b -
 color stmp color exch color exch tmp setrgbcolor } def
/t { show } def
/savedmatrix [0 0 0 0 0 0] def
/savematrix { savedmatrix currentmatrix pop } def
/restorematrix { savedmatrix setmatrix } def
%% ISO fonts
/ISOLatin1Encoding [
 /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
 /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
 /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
 /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
 /space /exclam /quotedbl /numbersign /dollar /percent /ampersand /quoteright
 /parenleft /parenright /asterisk /plus /comma /minus /period /slash
 /zero /one /two /three /four /five /six /seven
 /eight /nine /colon /semicolon /less /equal /greater /question
 /at /A /B /C /D /E /F /G /H /I /J /K /L /M /N /O
 /P /Q /R /S /T /U /V /W /X /Y /Z /bracketleft /backslash /bracketright
                                                       /asciicircum /underscore
 /quoteleft /a /b /c /d /e /f /g /h /i /j /k /l /m /n /o
 /p /q /r /s /t /u /v /w /x /y /z /braceleft /bar /braceright /asciitilde
                                                                       /.notdef
 /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
 /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
 /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
 /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
 /space /exclamdown /cent /sterling /currency /yen /brokenbar /section
 /dieresis /copyright /ordfeminine /guillemotleft /logicalnot /hyphen
                                                            /registered /macron
 /degree /plusminus /twosuperior /threesuperior /acute /mu /paragraph
                                                                /periodcentered
 /cedilla /onesuperior /ordmasculine /guillemotright /onequarter /onehalf
                                                   /threequarters /questiondown
 /Agrave /Aacute /Acircumflex /Atilde /Adieresis /Aring /AE /Ccedilla
 /Egrave /Eacute /Ecircumflex /Edieresis /Igrave /Iacute /Icircumflex
                                                         /Idieresis
 /Eth /Ntilde /Ograve /Oacute /Ocircumflex /Otilde /Odieresis /multiply
 /Oslash /Ugrave /Uacute /Ucircumflex /Udieresis /Yacute /Thorn /germandbls
 /agrave /aacute /acircumflex /atilde /adieresis /aring /ae /ccedilla
 /egrave /eacute /ecircumflex /edieresis /igrave /iacute /icircumflex
                                                         /idieresis
 /eth /ntilde /ograve /oacute /ocircumflex /otilde /odieresis /divide
 /oslash /ugrave /uacute /ucircumflex /udieresis /yacute /thorn /ydieresis
] def
%% usage: isoname oldname makeisofont -
/makeisofont {
  dup findfont length dict dup begin
    exch findfont {
      exch dup /FID eq { pop pop } { exch def } ifelse
    } forall
    /Encoding ISOLatin1Encoding def
  end
  definefont pop
} def
";;

let make_iso_font f = sprintf
 "/ISO%s /%s makeisofont" f f;;

let hashtbl_assocs t =
 let res = ref [] in
 Hashtbl.iter (fun k v -> res := (k, v) :: !res) t;
 !res;;

let hashtbl_vals t =
 let res = ref [] in
 Hashtbl.iter (fun k v -> res := v :: !res) t;
 !res;;

let hashtbl_map f t =
 let res = ref [] in
 Hashtbl.iter (fun k v -> res := f k v :: !res) t;
 !res;;

let make_iso_fonts () =
 let font_list = hashtbl_vals fonts in
 String.concat "\n" (List.map make_iso_font font_list);;

let set_default_font default_font default_font_size = sprintf
 "/ISO%s findfont %i scalefont setfont\n" default_font default_font_size;;

let creator x0 y0 width height = sprintf
  "%%!PS-Adobe-2.0 EPSF-2.0\n\
   %%%%Creator: GraphPs %s\n\
   %%%%BoundingBox: %i %i %i %i\n" graphps_version x0 y0 width height;;

let document_fonts () =
  let font_list = hashtbl_vals fonts in
  sprintf
   "%%%%DocumentFonts: %s\n\
    %%%%EndComments\n" (String.concat " " font_list);;

let header x0 y0 width height =
  sprintf "%s%s" (creator x0 y0 width height) (document_fonts ());;

let prelude x0 y0 width height default_font default_font_size comment =
  sprintf
   "%s\
    %%%%%s\n\
    %%BeginProcSet\n\
    gsave\n\
    %s\
    0 0 m\n\
    1 setlinewidth\n\
    1 setlinecap\n\
    2 setlinejoin\n\
    10 setmiterlimit\n\
    %s\n\
    %s\
    %%EndProcSet\n" (header x0 y0 width height)
        comment
        ps_defs
        (make_iso_fonts ())
        (set_default_font default_font default_font_size);;

(* Could be "showpage", if one wants automatic display of the current page. *)
let postlude () =
 if !eps_mode then "grestore\n" else "grestore\nshowpage\n%%end\n";;

let escape_char b = function
  | '(' | ')' | '\\' as c -> Buffer.add_char b '\\'; Buffer.add_char b c
  | c -> Buffer.add_char b c;;

let escape_char_for_ps c =
 let b = Buffer.create 3 in
 escape_char b c;
 Buffer.contents b;;

let escape_string_for_ps s =
 let l = String.length s in
 let b = Buffer.create l in
 for i = 0 to l - 1 do escape_char b s.[i] done;
 Buffer.contents b;;

type filling = Fill | Draw;;

(*i
let print_poly filling v =
  if Array.length v > 0 then begin
   let x, y = v.(0) in
   let s1 = sprintf "c %i %i pc\n" x y
   in
   s1 @
   for i = 1 to Array.length v - 1 do
     let x, y = v.(i) in
     fprintf oc "%i %i l\n" x y
   done;
   (if filling = Draw then "dp\n" else "fp\n")
  end;;
i*)

let print_poly filling t =
  let rec h i acc =
    if i=Array.length t then acc 
    else h (i+1) (let x,y = t.(i) in (sprintf "%i %i l" x y) :: acc)
  in
  let x,y = t.(0) in
  String.concat "\n" ((sprintf "c %i %i pc" x y) ::
		      (List.rev (h 1 [])) @
		     (if filling = Draw then [ "stroke m\n" ] else [ "fp\n" ]))
;;

(*i already done before -- escape_string_for_ps
(* some characters must be escaped in text elements to convert them to
   a postscript text element. *)

let rec explode s =
  if s = "" then [] else
  s.[0] :: explode (String.sub s 1 (String.length s - 1))

let soc = Printf.sprintf "%c"

let ps_escape s =
  let esc c = match c with
  | '(' -> "\("
  | ')' -> "\)"
   (* others ? ZZZ *)
  | c   -> soc c
  in
  String.concat "" (List.map esc (explode s))
i*)

let ps_of_cmd = function
  | Open_pict (x0, y0, w, h) -> sprintf "%s\n" "%% hohoho!"
  | Close_pict -> sprintf "%s\n" (postlude ())

  | CText(x,y,s) -> sprintf "%i %i m (%s) ctext\n" x y (escape_string_for_ps s)
  | Segment(x1,y1,x2,y2) -> sprintf "%i %i m %i %i dl\n" x1 y1 x2 y2

  | Set_rgb_color (r, g, b) ->
     sprintf "%i %i %i srgb\n" r g b
  | Plot (x, y) ->
     sprintf "cp %i %i p\n" x y
  | Moveto (x, y) ->
     sprintf "%i %i m\n" x y
  | Lineto (x, y) ->
     sprintf "%i %i dl\n" x y
  | Rmoveto (x, y) ->
     sprintf "%i %i rm\n" x y
  | Rlineto (x, y) ->
     sprintf "%i %i rl\n" x y
  | Curveto ((x1, y1), (x2, y2), (x3, y3)) ->
     sprintf "%i %i %i %i %i %i ct\n" x1 y1 x2 y2 x3 y3
  | Set_line_width w ->
     sprintf "%i setlinewidth\n" w
(*i  | Draw_char c ->
     sprintf "(%s) show\n" (escape_char_for_ps c)
  | Draw_string s ->
     sprintf "(%s) show\n" (escape_string_for_ps s)
  | Set_font (f, size) ->
     sprintf "/ISO%s findfont %i scalefont setfont\n" f size
i*)  | Draw_rect (x, y, w, h) ->
     sprintf "c %i %i %i %i dr\n" w h x y
  | Fill_rect (x, y, w, h) ->
     sprintf "c %i %i %i %i fr\n" w h x y
(*i  | Fill_poly v ->
     print_poly oc Fill v
i*)  | Draw_poly v ->
     print_poly Draw v
  | Draw_circle (x, y, r) ->
     sprintf "c %i %i %i dc\n" x y r
  | Fill_circle (x, y, r) ->
     sprintf "c %i %i %i fc\n" x y r
  | Draw_ellipse (x, y, rx, ry) ->
     sprintf "%i %i %i %i de\n" rx ry x y
  | Fill_ellipse (x, y, rx, ry) ->
     sprintf "%i %i %i %i fe\n" rx ry x y
  | Draw_arc (x, y, rx, ry, a1, a2) ->
     sprintf "0 0 1 %i %i %i %i %i %i da\n" a1 a2 rx ry x y
  | Fill_arc (x, y, rx, ry, a1, a2) ->
     sprintf "0 0 1 %i %i %i %i %i %i fa\n" a1 a2 rx ry x y

;;

let to_eps oc pict comment =
  match pict with { cmds=c; x=x; y=y; width=w; height=h } ->
    output_string oc (sprintf "%s" (prelude x y (x+w) (y+h) 
			      default_font default_font_size comment));
    List.iter (fun c -> output_string oc (ps_of_cmd c)) (List.rev c)
  

let to_eps_file name comment pict =
  let oc = open_out name
  in
  to_eps oc pict comment; close_out oc


(* ================ xfig export

Just the bare minimum to export a tree.  Goal is to handle latex text
in tree labels.

 *)


(* Handling colors.  We need to define color objects at beginning of
file.  [get_all_colors] adds all definitions in [colors] hashtable.
[define_colors] generate xfig string for the color definitions.

For simplicity we (currently?) use only user colors.  Note that xfig
has 32 predefined colors and does not support more than 512 user
colors.  *)

let colors = Hashtbl.create 30
let max_color = ref 32

let add_color r g b =
  try
    Hashtbl.find colors (r,g,b); ()
  with Not_found ->
    Hashtbl.add colors (r,g,b) !max_color;
    incr max_color

let get_color r g b =
  Hashtbl.find colors (r,g,b)

let define_colors () =
  let hexcolor r g b = Printf.sprintf "#%02x%02x%02x" r g b
  in
  Hashtbl.fold (fun (r,g,b) c acc -> Printf.sprintf "0 %i %s\n" c (hexcolor r g b) ^ acc)
    colors ""

let get_all_colors cmds =
  List.iter 
    (fun c -> match c with Set_rgb_color(r,g,b) -> add_color r g b | _ -> ())
    cmds

let tolatex=ref true

let yfigmax = 1200. *. 4.  (*8.25*)


(* vérifier header *)
let fig_header name =
  "#FIG 3.2\nLandscape\nCenter\nMetric\nA4\n100.00\nSingle\n-2\n"
  ^ "#Created by pictures " ^ picture_version
  ^ "\n#for file " ^ name  ^ "\n1200 2\n"

let scale = 16 (*yfigmax /. (!ymax-. !ymin)*)

let f x = (x*scale)
let fy y = (int_of_float yfigmax) - (f y)
let fyt y = (int_of_float yfigmax) - (f y) - (6*scale) (* ZZZ *)

let xfig_string st =  st ^ "\\001"

let style_of_graph_ fill g =
  let code_ls _  = 0
  in
  let c = 0 in
  (* for polyline *)
  (* line style, thickness, pen_color, fill_color, depth, pen_style, area_fill, style_val *)
  sprintf "%i %i %i %i 50 -1 %i 5" (code_ls ()) 1 c c
    (if fill then 20 else -1)

let fig_color = ref 0

let style_of_graph = style_of_graph_ false

let fail s = failwith (s ^ ": element unsupported for fig export") 

let fig_of_cmd = function
  | Open_pict (x0, y0, w, h) -> sprintf "%s\n" "%% hohoho!"
  | Close_pict -> ""

  | CText(x,y,s) -> 
      let text t c x y =
	(* 5 = typewriter (12 = courier), 10.0 = taille *)
	sprintf "4 1 %i 50 -1 5 10.0 0.0 %i 0.0 0.0 %i %i %s\n" c
	  (if !tolatex then 3 else 1) (f x) ((fy y)(*+60*))(*ZZZ*) (xfig_string t)
      in text s !fig_color x y

  | Segment(x1,y1,x2,y2) ->
      let vline g c x1 y1 x2 y2 = 
	sprintf "2 1 %s 0 0 0 0 0 2\n\t %i %i %i %i\n" (style_of_graph g)
	  (f x1) (fy y1) (f x2) (fy y2)
      in vline () !fig_color x1 y1 x2 y2

  | Set_rgb_color (r, g, b) -> fig_color := get_color r g b; ""

  | Plot (x, y) -> fail "plot"
  | Moveto (x, y) -> fail "mt"
  | Lineto (x, y) -> fail "lt"
  | Rmoveto (x, y) -> fail "rmt"
  | Rlineto (x, y) -> fail "rlt"
  | Curveto ((x1, y1), (x2, y2), (x3, y3))  -> fail "ct"
  | Set_line_width w -> fail "w"
(*i  | Draw_char c ->
     sprintf "(%s) show\n" (escape_char_for_ps c)
  | Draw_string s ->
     sprintf "(%s) show\n" (escape_string_for_ps s)
  | Set_font (f, size) ->
     sprintf "/ISO%s findfont %i scalefont setfont\n" f size
i*)  | Draw_rect (x, y, w, h) -> fail "dr"

  | Fill_rect (x, y, w, h) ->
      (* we set the fill_color, fill intensity = 20 = 100 % *)
      sprintf "2 2 0 0 0 %i 50 0 20 0 0 0 0 0 0 5\n\t%i %i %i %i %i %i %i %i %i %i\n"
	!fig_color 
	(f x) (fy y) (f (x+w)) (fy y) (f (x+w)) (fy (y+h)) (f x) (fy (y+h)) (f x) (fy y)

(*i  | Fill_poly v ->
     print_poly oc Fill v
i*)  | Draw_poly v  -> fail "dp"
  | Draw_circle (x, y, r) -> fail "dc"

  | Fill_circle (x, y, r) -> fail "fc"

  | Draw_ellipse (x, y, rx, ry) -> fail "de"
  | Fill_ellipse (x, y, rx, ry) -> fail "fe"
  | Draw_arc (x, y, rx, ry, a1, a2) -> fail "da"
  | Fill_arc (x, y, rx, ry, a1, a2) -> fail "fa"


let to_fig oc pict comment =
  match pict with { cmds=c; x=x; y=y; width=w; height=h } ->
    get_all_colors c;
    output_string oc (fig_header comment);
    output_string oc (define_colors ());
    List.iter (fun c -> output_string oc (fig_of_cmd c)) (List.rev c)
  

let to_fig_file name comment pict =
  let oc = open_out name
  in
  to_fig oc pict comment; close_out oc




(* UNUSED ??? *)

(* Drawing *)

let plots points =
  for i = 0 to Array.length points - 1 do
    let (x, y) = points.(i) in
    plot x y
  done
;;

let draw_poly_line =
  let draw points =
    if Array.length points > 0 then begin
      let (savex, savey) = current_point () in
      moveto (fst points.(0)) (snd points.(0));
      for i = 1 to Array.length points - 1 do
        let (x, y) = points.(i) in
        lineto x y;
      done;
      moveto savex savey;
    end in
  draw;;

let draw_segments segs =
  let (savex, savey) = current_point () in
  for i = 0 to Array.length segs - 1 do
    let (x1, y1, x2, y2) = segs.(i) in
    moveto x1 y1;
    lineto x2 y2;
  done;
  moveto savex savey;;


let transp = -1;;
