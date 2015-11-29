open Graphics
open GraphicsRipper
open Model

type controller = Human | Ai
type player = {
  cont: controller;
  color: color;
  mutable pos_x: int;
  mutable pos_y: int;
}

let list_players = []

(* Set up display *)
let graph = open_graph " 880x920"
let _ = auto_synchronize false
let _ = display_mode false
let _ = set_window_title "Quoridor"

(* Set up images *)
let wallHRegal = make_image (rip "WallH_Regal")
let wallVRegal = make_image (rip "WallV_Regal")
let wallCRegal = make_image (rip "WallC_Regal")
let boardRegal = make_image (rip "Board_Regal")
let pawnRegal = make_image (rip "Pawn_Regal")
let banner1Regal = make_image (rip "Banner1_Regal")
let banner2Regal = make_image (rip "Banner2_Regal")
let playerMenu = make_image (rip "player")
let huHi = make_image (rip "human")
let aiHi = make_image (rip "ai")
let colorWheel = make_image (rip "colorwheel")
let howTo = make_image (rip "howTo")
let menu = make_image (rip "menu")

(* Set up defaults *)
let save = ref (create_board 9)
let board_type = ref 0
let button = ref false

(* Convert an array y marking to pixel coordinate *)
let yf z =
  let inv1 = (16 - z)/2 in
  let inv = if (16 - z) = -1 then -1 else inv1 in (* compensate for / spec *)
  match z mod 2 with
    | 0 -> inv*100
    | _ -> inv*100 + 80

(* Convert an array x marking to pixel coordinates *)
let xf z =
  let inv = z/2 in
  match z mod 2 with
    | 0 -> inv*100
    | _ -> inv*100 + 80

(* draw a player's circle and their board covering *)
let draw_player player =
  set_color (player.color);
  fill_circle ((xf player.pos_x) + 40) ((yf player.pos_y) + 40) 30;
  let img = match !board_type with
    | 0 -> pawnRegal
    | _ -> pawnRegal
  in
  draw_image img ((xf player.pos_x) + 10) ((yf player.pos_y) + 10)

(* draw walls based on save ref *)
let draw_walls () =
  let board = (!save).board in
  let wallH = match !board_type with
    | _ -> wallHRegal
  in
  let wallV = match !board_type with
    | _ -> wallVRegal
  in
  let wallC = match !board_type with
    | _ -> wallCRegal
  in
  for i = 0 to 16 do
    if i mod 2 = 0 then
      for j = 0 to 7 do
        match board.(i).(j*2 + 1) with
          | Wall -> draw_image wallV (xf (j*2 + 1)) (yf i)
          | _ -> ()
      done
    else
      for j = 0 to 16 do
        match board.(i).(j) with
          | Wall -> (match j mod 2 with
                      | 0 -> draw_image wallH (xf j) (yf i)
                      | _ -> draw_image wallC (xf j) (yf i))
          | _ -> ()
      done
  done

(* draw a ghost wall *)
let rec draw_ghost (walls:loc list) =
  set_color (rgb 158 132 101);
  match walls with
    | [] -> ()
    | (y, x)::t -> match y mod 2 with
                    | 0 -> fill_rect (xf x) (yf y) 20 80; draw_ghost t
                    | _ -> match x mod 2 with
                            | 0 -> fill_rect (xf x) (yf y) 80 20; draw_ghost t
                            | _ -> fill_rect (xf x) (yf y) 20 20; draw_ghost t

(* draws the board *)
let draw_board () =
  match !board_type with
    | _ -> draw_image boardRegal 0 0

(* Draws everything (calls functions) *)
let draw ghosts players play synch =
  draw_board ();
  for i = 0 to ((List.length players) - 1) do
    draw_player (List.nth players i)
  done;
  draw_walls ();
  draw_ghost ghosts;
  (match play with
    | 0 -> draw_image banner1Regal 0 880
    | _ -> draw_image banner2Regal 0 880);
  if synch then synchronize graph else ()

(* Draws main menu *)
let drawMenu () =
  clear_graph graph;
  draw_image menu 0 0;
  synchronize graph

(* Draws How to Play *)
let drawHow () =
  clear_graph graph;
  draw_image howTo 0 0;
  synchronize graph

let drawPlayer (c1:controller) (c2:controller) (c:color) (c':color) (cw:bool) =
  clear_graph graph;
  draw_image playerMenu 0 0;
  (match c1 with
    | Human -> draw_image huHi 357 717
    | Ai -> draw_image aiHi 692 705);
  (match c2 with
    | Human -> draw_image huHi 368 440
    | Ai -> draw_image aiHi 703 427);
  set_color c;
  fill_rect 440 575 122 123;
  set_color c';
  fill_rect 440 299 122 122;
  (if cw then draw_image colorWheel 200 220 else ());
  synchronize graph

(* Iterates to next player *)
let next_player players player =
  let num = List.length players in
  (player + 1) mod num

(* Main game loop *)
let rec loop (players:player list) (cur_player:int) =
  (*let win = checkWin players in
  (if (fst win) then drawWin (snd win) else ());*)
  if (List.nth players cur_player).cont = Human then
   let event = wait_next_event [Poll] in
    let new_player =
      if (event.key = '\027') then (close_graph graph; exit 0)
      else if event.button && not(!button) then
        let modx = event.mouse_x mod 100 in
        let mody = event.mouse_y mod 100 in
        let my = abs(16 - ((event.mouse_y / 100) * 2)) -
                  if mody >= 80 then 1 else 0 in
        let mx = (event.mouse_x / 100) * 2 + if modx >= 80 then 1 else 0 in
        if(modx < 80 && mody < 80) then
          let mv = validate_move cur_player (Move(my,mx)) (!save) in
          if (fst mv) then
            let play = List.nth players cur_player in
            save := (snd mv);
            play.pos_x <- mx; play.pos_y <- my;
            let next = next_player players cur_player in
            draw [] players next true; next
          else
            let _ = draw [] players cur_player false in
            moveto 600 890;
            set_color white;
            draw_string "Invalid Move";
            synchronize graph;
            cur_player
        else
          let accept =
            if (mody >= 80 && modx >= 80) then false
            else if (mody <= 50 && modx > 80) then (* Down *)
              let l = [(my, mx);(my+1, mx);(my+2, mx)] in
              let mv = validate_move cur_player (PlaceWall(l)) (!save) in
              if (fst mv) then let _ = save := (snd mv) in true else false
            else if (modx <= 50 && mody > 80) then (* Left *)
              let l = [(my, mx);(my, mx-1);(my, mx-2)] in
              let mv = validate_move cur_player (PlaceWall(l)) (!save) in
              if (fst mv) then let _ = save := (snd mv) in true else false
            else if (modx > 80 && mody > 50) then (* Up *)
              let l = [(my, mx);(my-1, mx);(my-2, mx)] in
              let mv = validate_move cur_player (PlaceWall(l)) (!save) in
              if (fst mv) then let _ = save := (snd mv) in true else false
            else if (mody > 80 && modx > 50) then (* Right *)
              let l = [(my, mx);(my, mx+1);(my, mx+2)] in
              let mv = validate_move cur_player (PlaceWall(l)) (!save) in
              if (fst mv) then let _ = save := (snd mv) in true else false
            else false
          in
          if accept
            then
              let next = next_player players cur_player in
              draw [] players next true; next
            else
              let _ = draw [] players cur_player false in
              moveto 600 890;
              set_color white;
              draw_string "Invalid Wall Placement";
              synchronize graph;
              cur_player
      else
        let modx = event.mouse_x mod 100 in
        let mody = event.mouse_y mod 100 in
        let my = abs(16 - ((event.mouse_y / 100) * 2)) -
                  if mody >= 80 then 1 else 0 in
        let mx = (event.mouse_x / 100) * 2 + if modx >= 80 then 1 else 0 in
        let ghosts =
          if (mody >= 80 && modx >= 80) then [] (* center square *)
          else if (mody <= 50 && modx > 80) then (* Down *)
            [(my, mx);(my+1, mx);(my+2, mx)]
          else if (modx <= 50 && mody > 80) then (* Left *)
            [(my, mx);(my, mx-1);(my, mx-2)]
          else if (modx > 80 && mody > 50) then (* Up *)
            [(my, mx);(my-1, mx);(my-2, mx)]
          else if (mody > 80 && modx > 50) then (* Right *)
            [(my, mx);(my, mx+1);(my, mx+2)]
          else []
        in
        draw ghosts players cur_player true;
        cur_player
    in
    let _ = button := event.button in
    loop players new_player
  else
    let next = next_player players cur_player in
    let _ = save := ai_move cur_player (!save) in
    draw [] players next true; loop players next


(* Sets up players and initializes the game loop *)
let players (c1:controller) (c2:controller) (c:color) (c':color) =
  button := true;
  let pl_1 =
    {cont = c1; color = c; pos_x = 8; pos_y = 0} in
  let pl_2 =
    {cont = c2; color = c'; pos_x = 8; pos_y = 16} in
  loop [pl_1;pl_2] 0

(* Loop for instructions menu *)
let rec howToPlayLoop () =
  let event = wait_next_event [Poll] in
  if (event.key = '\027') then ignore(close_graph graph; exit 0)  else
  if (event.button && not(!button)) then
    let posx = event.mouse_x in
    let posy = event.mouse_y in
    if (posy <= 160) then
      if (posx <= 220) then
        menuInit ()
      else if (posx >= 660) then
        playerInit ()
      else
        howToPlayLoop ()
    else
      howToPlayLoop ()
  else button := event.button; howToPlayLoop ()

(* Initializes instructions Loop *)
and howToPlayLoopInit () =
  button := true; drawHow (); howToPlayLoop ()

(* Loop for the main menu *)
and menuLoop () =
  let event = wait_next_event [Poll] in
  if (event.key = '\027') then ignore(close_graph graph; exit 0)  else
  if (event.button && not(!button)) then
    let posx = event.mouse_x in
    let posy = event.mouse_y in
    if (posy >= 145 && posy <= 340) then
      if (posx >= 97 && posx <= 422) then
        playerInit ()
      else if (posx >= 470 && posx <= 784) then
        howToPlayLoopInit ()
      else
        menuLoop ()
    else
      menuLoop ()
  else button := event.button; menuLoop ()

and menuInit () = button := true; drawMenu (); menuLoop ()

and playerInit () = button := true; drawPlayer Human Human red green false;
    playerLoop Human Human red green false 0

and playerLoop
      (c1:controller) (c2:controller) (c:color) (c':color) (cw:bool) (id:int) =
  drawPlayer c1 c2 c c' cw;
  let event = wait_next_event [Poll] in
  if (event.key = '\027') then ignore(close_graph graph; exit 0)  else
  if cw then
    if (event.button && not(!button)) then
      let posx = event.mouse_x in
      let posy = event.mouse_y in
      if ((abs(posx - 440) * abs(posx - 440) +
           abs(posy - 460) * abs(posy - 460)) < 50625) then
        let col = point_color posx posy in
        match id with
          | 0 -> (button := true; playerLoop c1 c2 col c' false 0)
          | _ -> (button := true; playerLoop c1 c2 c col false 1)
      else
        playerLoop c1 c2 c c' cw id
    else
      (button := event.button; playerLoop c1 c2 c c' cw id)
  else if (event.button && not(!button)) then
    let posx = event.mouse_x in
    let posy = event.mouse_y in
    if (posy <= 100) then
      if (posx <= 200) then
        menuInit ()
      else if (posx >= 700) then
        players c1 c2 c c'
      else
       playerLoop c1 c2 c c' cw id
    else if (posy >= 710 && posy <= 775 && posx >= 358 && posx <= 490) then
      playerLoop Human c2 c c' cw id
    else if (posy >= 692 && posy <= 772 && posx >= 694 && posx <= 782) then
      playerLoop Ai c2 c c' cw id
    else if (posy >= 430 && posy <= 498 && posx >= 368 && posx <= 597) then
      playerLoop c1 Human c c' cw id
    else if (posy >= 415 && posy <= 496 && posx >= 704 && posx <= 793) then
      playerLoop c1 Ai c c' cw id
    else if (posy >= 574 && posy <= 702 && posx >= 436 && posx <= 565) then
      (button := true; playerLoop c1 c2 c c' true 0)
    else if (posy >= 294 && posy <= 426 && posx >= 436 && posx <= 565) then
      (button := true; playerLoop c1 c2 c c' true 1)
    else playerLoop c1 c2 c c' cw id
  else button := event.button; playerLoop c1 c2 c c' cw id

let _ = menuInit ()