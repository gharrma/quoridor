open Graphics
open GraphicsRipper
open Model

type trig = AiThink | MoveInvalid | WallInvalid | Blank
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
let wallH = make_image (rip "images/WallH")
let wallV = make_image (rip "images/WallV")
let wallC = make_image (rip "images/WallC")
let boardImg = make_image (rip "images/board")
let pawnCover = make_image (rip "images/pawnCover")
let ban1 = make_image (rip "images/player1")
let ban2 = make_image (rip "images/player2")
let banner = make_image (rip "images/banner")
let bannerAi = make_image (rip "images/bannerAi")
let bannerMove = make_image (rip "images/bannerMove")
let bannerWall = make_image (rip "images/bannerWall")
let playerMenu = make_image (rip "images/player")
let win = make_image (rip "images/win")
let win1temp = make_image (rip "images/play1Win")
let win2temp = make_image (rip "images/play2Win")
let huHi = make_image (rip "images/human")
let aiHi = make_image (rip "images/ai")
let colorWheel = make_image (rip "images/colorwheel")
let howTo = make_image (rip "images/howTo")
let menu = make_image (rip "images/menu")
let img0 = make_image (rip "images/0")
let img1 = make_image (rip "images/1")
let img2 = make_image (rip "images/2")
let img3 = make_image (rip "images/3")
let img4 = make_image (rip "images/4")
let img5 = make_image (rip "images/5")
let img6 = make_image (rip "images/6")
let img7 = make_image (rip "images/7")
let img8 = make_image (rip "images/8")
let img9 = make_image (rip "images/9")
let img10 = make_image (rip "images/10")
let enumImg = [img0;img1;img2;img3;img4;img5;img6;img7;img8;img9;img10]

(* Set up defaults *)
let save = ref (create_board 9)
let button = ref false
let instance = ref Blank
let play1ban = ref ban1
let play2ban = ref ban2
let win1 = ref win1temp
let win2 = ref win2temp

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
  draw_image pawnCover (xf player.pos_x) (yf player.pos_y)

(* draw walls based on save ref *)
let draw_walls () =
  let board = (!save).board in
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

(* Draws everything (calls functions) *)
let draw ghosts players play =
  clear_graph graph;
  draw_image boardImg 0 0;
  for i = 0 to ((List.length players) - 1) do
    draw_player (List.nth players i)
  done;
  draw_walls ();
  draw_ghost ghosts;
  (match !instance with
    | WallInvalid -> draw_image bannerWall 0 880
    | AiThink -> draw_image bannerAi 0 880
    | MoveInvalid -> draw_image bannerMove 0 880
    | Blank -> draw_image banner 0 880);
  (match play with
    | 0 -> draw_image (!play1ban) 100 887
    | _ -> draw_image (!play2ban) 100 887);
  let num = (snd ((!save).players.(play))) in
  draw_image (List.nth enumImg num) 500 894;
  synchronize graph


(* Draws main menu *)
let rec drawMenu () =
  clear_graph graph;
  draw_image menu 0 0;
  synchronize graph

(* Draws How to Play *)
and drawHow () =
  clear_graph graph;
  draw_image howTo 0 0;
  synchronize graph

and drawPlayer (c1:controller) (c2:controller) (c:color) (c':color) (cw:bool) =
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
and next_player players player =
  let num = List.length players in
  (player + 1) mod num

and checkWin players =
  if (List.nth players 0).pos_y = 16 then (true, 0) else
  if (List.nth players 1).pos_y = 0 then (true, 1) else
  (false, 3)

and waitWin () =
  let event = wait_next_event [Poll] in
  if (event.button && not(!button)) then
    let posx = event.mouse_x in
    let posy = event.mouse_y in
    if (posx >= 220 && posx <= 420 && posy >= 340 && posy <= 420) then
      playerInit ()
    else if (posx >= 460 && posx <= 660 && posy >= 340 && posy <= 420) then
      ignore(clear_graph graph; exit 0)
    else
      button := event.button; waitWin ()
  else
    button := event.button; waitWin ()

and drawWin play =
  draw_image win 0 0;
  (match play with
    | 0 -> draw_image (!win1) 252 467
    | _ -> draw_image (!win2) 252 467);
  synchronize graph; button := true; waitWin ()

(* Main game loop *)
and loop (players:player list) (cur_player:int) =
  let win = checkWin players in
  (if (fst win) then drawWin (snd win) else ());
  if (List.nth players cur_player).cont = Human then
   let event = wait_next_event [Button_down; Key_pressed; Mouse_motion] in
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
          if mv then
            let () = commit_move cur_player (Move(my,mx)) (!save) in
            let play = List.nth players cur_player in
            play.pos_x <- mx; play.pos_y <- my;
            let next = next_player players cur_player in
            instance := Blank; draw [] players next; next
          else
            let _ = instance := MoveInvalid; draw [] players cur_player in
            cur_player
        else
          let accept =
            if (mody >= 80 && modx >= 80) then false
            else if (mody <= 50 && modx > 80) then (* Down *)
              let l = [(my, mx);(my+1, mx);(my+2, mx)] in
              let mv = validate_move cur_player (PlaceWall(l)) (!save) in
              if mv then 
                let () = commit_move cur_player (PlaceWall(l)) (!save) in
                true
              else false
            else if (modx <= 50 && mody > 80) then (* Left *)
              let l = [(my, mx);(my, mx-1);(my, mx-2)] in
              let mv = validate_move cur_player (PlaceWall(l)) (!save) in
              if mv then
                let () = commit_move cur_player (PlaceWall(l)) (!save) in
                true
              else false
            else if (modx > 80 && mody > 50) then (* Up *)
              let l = [(my, mx);(my-1, mx);(my-2, mx)] in
              let mv = validate_move cur_player (PlaceWall(l)) (!save) in
              if mv then 
                let () = commit_move cur_player (PlaceWall(l)) (!save) in
                true
              else false
            else if (mody > 80 && modx > 50) then (* Right *)
              let l = [(my, mx);(my, mx+1);(my, mx+2)] in
              let mv = validate_move cur_player (PlaceWall(l)) (!save) in
              if mv then
                let () = commit_move cur_player (PlaceWall(l)) (!save) in
                true 
              else false
            else false
          in
          if accept
            then
              let next = next_player players cur_player in
              instance := Blank;
              draw [] players next; next
            else
              let _ = instance := WallInvalid; draw [] players cur_player in
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
        draw ghosts players cur_player; cur_player
    in
    let _ = button := event.button in
    loop players new_player
  else
    let next = next_player players cur_player in
    instance := AiThink; draw [] players cur_player;
    let () = commit_move cur_player (Ai.next_move !save cur_player) !save in
    let play = List.nth players cur_player in
    play.pos_x <- (snd (fst (!save).players.(cur_player)));
    play.pos_y <- (fst (fst (!save).players.(cur_player)));
    instance := Blank; draw [] players next; loop players next


(* Sets up players and initializes the game loop *)
and players (c1:controller) (c2:controller) (c:color) (c':color) =
  button := true;
  play1ban := (make_image (ripColorize "images/player1" c));
  play2ban := (make_image (ripColorize "images/player2" c'));
  win1 := (make_image (ripColorize "images/play1Win" c));
  win2 := (make_image (ripColorize "images/play2Win" c'));
  let pl_1 =
    {cont = c1; color = c; pos_x = 8; pos_y = 0} in
  let pl_2 =
    {cont = c2; color = c'; pos_x = 8; pos_y = 16} in
  loop [pl_1;pl_2] 0

(* Loop for instructions menu *)
and howToPlayLoop () =
  let event = wait_next_event [Button_down; Key_pressed; Mouse_motion] in
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
  let event = wait_next_event [Button_down; Key_pressed; Mouse_motion] in
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

and playerInit () =
  button := true; drawPlayer Human Human red green false;
  save := (create_board 9); playerLoop Human Human red green false 0

and playerLoop
      (c1:controller) (c2:controller) (c:color) (c':color) (cw:bool) (id:int) =
  drawPlayer c1 c2 c c' cw;
  let event = wait_next_event [Button_down; Key_pressed; Mouse_motion] in
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