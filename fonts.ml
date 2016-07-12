
open Graphics

let text = "ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFG"
let l = String.length text

let get_text_size font =
  set_font font;
  text_size text
;;

let fdiv a b = float_of_int a /. float_of_int b

let one_font fmt sizes =
  print_endline fmt;
  let f = Scanf.format_from_string fmt "%i" in
  List.iter (fun s -> 
    let (w,h) = get_text_size (Printf.sprintf f s) in
    Printf.printf "s=%i w=%i %.2f h=%i\n" s w (fdiv w l) h) 
    sizes
;;

open_graph "";

print_endline "default font";
let (w,h) = text_size text in Printf.printf "w=%i %.2f h=%i\n" w (fdiv w l) h;

one_font "lucidasans-%i" [8;10;12;14;18;24];

one_font "-misc-fixed-medium-r-normal--%i-*" [ 6; 7; 8; 9; 10; 13; 14; 15; 18; 20 ]
