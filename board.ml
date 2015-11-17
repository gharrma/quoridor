open Graphics
open Async

type direction = Left | Right | Up | Down | None
type wall = {x: int; y: int; dir: direction}

let graph = open_graph " 880x880"
let _ = auto_synchronize false
let _ = display_mode false
let x_1 = ref 1000
let x_2 = ref 1000
let y_1 = ref 1000
let y_2 = ref 1000

let draw_box x y size =
  set_color (rgb 207 207 207);
  let start_x = x * 100 in
  let start_y = y * 100 in
  fill_rect start_x start_y size size

let draw_player x y player =
  let colorPlayer = match player with
              | 1 -> blue
              | _ -> green
            in
  let x_cons = x / 100 in
  let y_cons = y / 100 in
  if (x_cons > 8 || x_cons < 0 || y_cons > 8 || y_cons < 0)
    then ()
    else set_color (colorPlayer);
         fill_circle (x_cons * 100 + 40) (y_cons * 100 + 40) 30

let rec draw_walls (walls:wall list) =
  set_color (rgb 153 87 11);
  match walls with
    | [] -> ()
    | h::t -> let _ = match h.dir with
              |Left -> fill_rect (h.x - 100) h.y 180 20
              |Right -> fill_rect h.x h.y 180 20
              |Up -> fill_rect h.x h.y 20 180
              |Down -> fill_rect h.x (h.y - 100) 20 180
              |None -> ()
            in draw_walls t

let draw_ghost (w:wall) =
  set_color (rgb 158 132 101);
  match w.dir with
    |Left -> fill_rect (w.x - 100) w.y 180 20
    |Right -> fill_rect w.x w.y 180 20
    |Up -> fill_rect w.x w.y 20 180
    |Down -> fill_rect w.x (w.y - 100) 20 180
    |None -> ()

let draw_board () =
  clear_graph graph;
  for i = 0 to 8 do
    for j = 0 to 8 do
      draw_box i j 80
    done
  done

let lst = [Button_down; Key_pressed; Mouse_motion]

let rec loop player walls=
  let new_walls = ref walls in
  let cur = wait_next_event lst in
  let new_player =
    (if cur.keypressed then
      match cur.key with
        |'a' -> 1
        |'\027' -> exit 0
        | _  -> 2
    else if cur.button then
      let mx = cur.mouse_x / 100 in
      let my = cur.mouse_y / 100 in
      let sidex = cur.mouse_x mod 100 in
      let sidey = cur.mouse_y mod 100 in
      let _ = if (sidex < 80 && sidey < 80) then
        if player = 1 then
          (x_1 := cur.mouse_x;
          y_1 := cur.mouse_y)
        else
          (x_2 := cur.mouse_x;
          y_2 := cur.mouse_y)
      else
        let temp_wall =
          if (sidey <= 50 && sidex > 50) then
            {x = mx*100 + 80; y = my*100; dir = Down} else
          if (sidex <= 50 && sidey > 50) then
            {x = mx*100; y = my*100 + 80; dir = Left} else
          if (sidex > 80 && sidey > 50) then
            {x = mx*100 + 80; y = my*100; dir = Up} else
          if (sidey > 80 && sidex > 50) then
            {x = mx*100; y = my*100 + 80; dir = Right} else{x= 0;y= 0;dir= None}
        in
        new_walls := temp_wall::!new_walls
      in
      draw_board ();
      draw_player !x_1 !y_1 1;
      draw_player !x_2 !y_2 2;
      draw_walls !new_walls;
      player
    else
      let mx = cur.mouse_x / 100 in
      let my = cur.mouse_y / 100 in
      let sidex = cur.mouse_x mod 100 in
      let sidey = cur.mouse_y mod 100 in
      let temp_wall =
        if (sidex < 80 && sidey < 80) then {x= 0;y= 0;dir= None} else
        if (sidey <= 50 && sidex > 50) then
          {x = mx*100 + 80; y = my*100; dir = Down} else
        if (sidex <= 50 && sidey > 50) then
          {x = mx*100; y = my*100 + 80; dir = Left} else
        if (sidex > 80 && sidey > 50) then
          {x = mx*100 + 80; y = my*100; dir = Up} else
        if (sidey > 80 && sidex > 50) then
          {x = mx*100; y = my*100 + 80; dir = Right} else {x= 0;y= 0;dir= None}
      in
      draw_board ();
      draw_player !x_1 !y_1 1;
      draw_player !x_2 !y_2 2;
      draw_walls !new_walls;
      draw_ghost temp_wall; player)
  in
  synchronize graph;
  loop new_player !new_walls

let _ = draw_board (); loop 1 []