open Graphics
open GraphicsRipper

let x = open_graph ""
let _ = resize_window 880 920
let _ = set_window_title "Quoridor"
let _ = display_mode false
let _ = auto_synchronize false

let menu = make_image (rip "menu")
let lst = [Poll]
let button = ref false

let draw s =
  clear_graph x;
  draw_image menu 0 0;
  moveto 400 500;
  draw_string s;
  synchronize x


let rec loop ()=
  let event = wait_next_event lst in
  if (event.key = 'q') then ignore(close_graph x; exit 0)  else
  if (event.button && not(!button)) then
    let posx = event.mouse_x in
    let posy = event.mouse_y in
    if (posy >= 145 && posy <= 340) then
      if (posx >= 97 && posx <= 422) then
        draw "Play button pressed"
      else if (posx >= 470 && posx <= 784) then
        draw "How2Play pressed"
      else
        draw "You missed"
    else
      draw ""
  else draw (string_of_bool(key_pressed x)) ;button := event.button; loop ()

let _ = draw ""; loop ()