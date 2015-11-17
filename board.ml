open Graphics

type direction = Left | Right | Up | Down | None
type wall = {x: int; y: int; dir: direction}
type controller = Human | Ai
type player = {
  cont: controller;
  color: color;
  mutable num_walls: int;
  mutable pos_x: int;
  mutable pos_y: int}

let lst_players = []

let graph = open_graph " 880x920"
let _ = auto_synchronize false
let _ = display_mode false

let draw_box x y size =
  set_color (rgb 207 207 207);
  let start_x = x * 100 in
  let start_y = y * 100 in
  fill_rect start_x start_y size size

let draw_player player =
  set_color (player.color);
  fill_circle (player.pos_x * 100 + 40) (player.pos_y * 100 + 40) 30

let rec draw_walls (walls:wall list) =
  set_color (rgb 153 87 11);
  match walls with
    | [] -> ()
    | h::t -> let _ = match h.dir with
              |Left -> fill_rect (h.x*100 - 100) (h.y*100 + 80) 180 20
              |Right -> fill_rect (h.x*100) (h.y*100 + 80) 180 20
              |Up -> fill_rect (h.x*100 + 80) (h.y*100) 20 180
              |Down -> fill_rect (h.x*100 + 80) (h.y*100 - 100) 20 180
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

let next_player players player =
  let num = List.length players - 1 in
  let new_val_attempt = player + 1 in
  if (new_val_attempt > num) then 0 else new_val_attempt

let draw_everything ghost walls players ghost_wall play=
  draw_board ();
  for i = 0 to ((List.length players) - 1) do
    draw_player (List.nth players i)
  done;
  draw_walls walls;
  (if ghost then draw_ghost ghost_wall else ());
  moveto 400 890;
  let player = List.nth players play in
  set_color (player.color);
  draw_string ("Current Player: "^ string_of_int(play));
  synchronize graph

let rec loop (players:player list) (walls:wall list) (cur_player: int) =
  let new_walls = ref walls in
  let cur = wait_next_event lst in
  let new_player =
    if cur.keypressed then
      match cur.key with
        | '\027' -> exit 0
        | _ -> cur_player
    else if cur.button then
      (let mx = cur.mouse_x / 100 in
      let my = cur.mouse_y / 100 in
      let sidex = cur.mouse_x mod 100 in
      let sidey = cur.mouse_y mod 100 in
      if (sidex < 80 && sidey < 80) then (
        if (mx  >= 0 && mx < 9 && my >= 0 && my < 9) (* Validate move player *) then
          let play = List.nth players cur_player in
          play.pos_x <- mx; play.pos_y <- my;
          let next = next_player players cur_player in
          draw_everything false (!new_walls) players {x = 0; y = 0; dir = None} next;
          next
        else
          (print_string("Invalid move, move again!\n");
          draw_everything false (!new_walls) players {x = 0; y = 0; dir = None} cur_player;
          cur_player ))
      else
        (if (mx  >= 0 && mx < 9 && my >= 0 && my < 9) (* Validate wall placement *) then
          let temp_wall =
            if (sidey <= 50 && sidex > 50) then
              {x = mx; y = my; dir = Down}::[] else
            if (sidex <= 50 && sidey > 50) then
              {x = mx; y = my; dir = Left}::[] else
            if (sidex > 80 && sidey > 50) then
              {x = mx; y = my; dir = Up}::[] else
            if (sidey > 80 && sidex > 50) then
              {x = mx; y = my; dir = Right}::[] else []
          in
          new_walls := temp_wall@(!new_walls);
          let play = List.nth players cur_player in
          play.num_walls <- play.num_walls - 1;
          let next = next_player players cur_player in
          draw_everything false (!new_walls) players {x = 0; y = 0; dir = None} next;
          next
        else
          (print_string("Invalid move, move again!\n");
          draw_everything false (!new_walls) players {x = 0; y = 0; dir = None} cur_player;
          cur_player)))
    else
      let mx = cur.mouse_x / 100 in
      let my = cur.mouse_y / 100 in
      let sidex = cur.mouse_x mod 100 in
      let sidey = cur.mouse_y mod 100 in
      let temp_wall =
        if (mx  >= 0 && mx < 9 && my >= 0 && my < 9) then
        if (sidex < 80 && sidey < 80) then ({x= 0;y= 0;dir= None}, false) else
        if (sidey <= 50 && sidex > 50) then
          ({x = mx*100 + 80; y = my*100; dir = Down}, true) else
        if (sidex <= 50 && sidey > 50) then
          ({x = mx*100; y = my*100 + 80; dir = Left}, true) else
        if (sidex > 80 && sidey > 50) then
          ({x = mx*100 + 80; y = my*100; dir = Up}, true) else
        if (sidey > 80 && sidex > 50) then
          ({x = mx*100; y = my*100 + 80; dir = Right}, true) else
          ({x= 0;y= 0;dir= None}, false)
        else ({x= 0;y= 0;dir= None}, false)
      in
      (if (snd temp_wall) then
        draw_everything true (!new_walls) players (fst temp_wall) cur_player
      else draw_everything false (!new_walls) players {x = 0; y = 0; dir = None} cur_player
      );
      cur_player
  in
  loop players !new_walls new_player

let _ =
  let player1 = {cont = Human; color = blue; pos_x = 4; pos_y = 0; num_walls = 5} in
  let player2 = {cont = Human; color = green; pos_x = 4; pos_y = 8; num_walls = 5} in
  draw_board (); synchronize graph;
  loop [player1;player2] [] 0
