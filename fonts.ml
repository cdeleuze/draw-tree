
open Graphics

let text = "ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFG"

let get_text_size font =
  set_font font;
  text_size text
;;


let one_font fmt sizes =
  print_endline fmt;
  let f = format_of_string (Obj.magic fmt) in
  List.iter (fun s -> 
    let (w,h) = get_text_size (Printf.sprintf f s) in Printf.printf "s=%i w=%i h=%i\n" s w h) 
    sizes
;;

open_graph "";

print_endline "default font";
let (w,h) = text_size text in Printf.printf "w=%i h=%i\n" w h;

one_font "lucidasans-%i" [8;10;12;14;18;24];

one_font "-misc-fixed-medium-r-normal--%i-*" [ 6; 7; 8; 9; 10; 13; 14; 15; 18; 20 ]




