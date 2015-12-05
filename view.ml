open Graphics
open Model

(* Variant that represents current state of board *)
type trig = AiThink | MoveInvalid | WallInvalid | Blank
type controller = Human | Ai
type player = { (* Represents a player character *)
  cont: controller;
  color: color;
  mutable pos_x: int;
  mutable pos_y: int;
}

(* Set up display *)
let graph = open_graph " 880x920"
let _ = auto_synchronize false
let _ = display_mode false
let _ = set_window_title "Quoridor"

(* Function that it utilized with rip and ripColorize to remove either the
   height or width of an image for a pbm file and return it
   Precondition: channel is currently at the start of either height or width
   Postcondition: returns an int representing the height/width and returns
                  with channel 1 step after '\n' *)
let rec hwrip chan a =
  match (input_char chan) with
    |'\n' -> int_of_string(a)
    | v   -> hwrip chan (a^Char.escaped v)

(* Applies a mathematical piecewise function to simulate an overlay effect
   of r/g/b channel representing c2 overlaying c1
   Precondtion: c1 and c2 are numbers 0-255
   Postcondition: Return new value of the effect of overlaying c2 onto c1
  Adapted from:
    http://www.deepskycolors.com/archive/2010/04/21/formulas-for-Photoshop-blending-modes.html
  for 0-255 numbers instead of 0-1*)
let overlay (c1:int) (c2:int) =
  if c1 < 128
  then int_of_float(float_of_int((c1*c2))/.127.5)
  else 255 - int_of_float(float_of_int((255-c1)*(255-c2))/.127.5)

(* Takes in a string for a .pbm file and returns a color array array that
   represents the image
   Precondition: name is a name.pbm file that exists in standard format of .pbm
   Postcondition: returns color array array matching image based on color spec
                  from graphics library. All "255/0/255" colors are transformed
                  into a transparent color *)
let rip (name:string) =
  let file = name^".pbm" in
  let ic = open_in file in
  for i = 0 to 2 do (* Rip PBM header *)
    ignore(input_char ic);
  done;
  let w = hwrip ic "" in
  let h = hwrip ic "" in
  for i = 0 to 3 do (* Rip color header *)
    ignore(input_char ic);
  done;
  let z = Array.make_matrix h w white in
  for i = 0 to (h - 1) do
    for j = 0 to (w - 1) do
      let r = int_of_char(input_char ic) in
      let g = int_of_char(input_char ic) in
      let b = int_of_char(input_char ic) in
      let rgbColor = rgb r g b in
      z.(i).(j) <- if rgbColor = 16711935 then (-1) else rgbColor;
    done;
  done;
  close_in ic;
  z

(* Takes in a string for a .pbm file and returns a color array array that
   represents the image with an overlay of col
   Precondition: name is a name.pbm file that exists in standard format of .pbm
                 and col is a color
   Postcondition: returns color array array matching image based on an overlay
                  effect of col onto the image, based on color spec from
                  graphics library. All "255/0/255" colors are transformed into
                  a transparent color) *)
let ripColorize (name:string) (col:color) =
  let bb1 = col mod 16 in
  let bb2 = (col/16) mod 16 in
  let gb1 = (col/256) mod 16 in
  let gb2 = (col/4096) mod 16 in
  let rb1 = (col/65536) mod 16 in
  let rb2 = (col/1048576) mod 16 in
  let blue = (bb1*16) + bb2 in
  let green = (gb1*16) + gb2 in
  let red = (rb1*16) + rb2 in
  let file = name^".pbm" in
  let ic = open_in file in
  for i = 0 to 2 do (* Rip PBM header *)
    ignore(input_char ic);
  done;
  let w = hwrip ic "" in
  let h = hwrip ic "" in
  for i = 0 to 3 do (* Rip color header *)
    ignore(input_char ic);
  done;
  let z = Array.make_matrix h w white in
  for i = 0 to (h - 1) do
    for j = 0 to (w - 1) do
      let r = int_of_char(input_char ic) in
      let g = int_of_char(input_char ic) in
      let b = int_of_char(input_char ic) in
      let rgbColor = rgb r g b in
      let r2 = overlay r red in
      let g2 = overlay g green in
      let b2 = overlay b blue in
      let rgbColor2 = rgb r2 g2 b2 in
      z.(i).(j) <- if rgbColor = 16711935 then (-1) else rgbColor2;
    done;
  done;
  close_in ic;
  z

(* Set up images based on above specifications*)
let wallH = make_image (rip "images/WallH") (* Horizontal Wall *)
let wallV = make_image (rip "images/WallV") (* Vertical Wall *)
let wallC = make_image (rip "images/WallC") (* Central Wall *)
let boardImg = make_image (rip "images/board") (* Main board *)
let pawnCover = make_image (rip "images/pawnCover") (* Pawn Cover *)
let ban1 = make_image (rip "images/player1") (* Banner player 1:no color *)
let ban2 = make_image (rip "images/player2") (* Banner player 2:no color *)
let banner = make_image (rip "images/banner") (* Banner base *)
let bannerAi = make_image (rip "images/bannerAi") (* Banner AI move *)
let bannerMove = make_image (rip "images/bannerMove") (* Banner invalid move *)
let bannerWall = make_image (rip "images/bannerWall") (* Banner invalid wall *)
let playerMenu = make_image (rip "images/player") (* Player customization img *)
let win = make_image (rip "images/win") (* Win overlay *)
let win1temp = make_image (rip "images/play1Win") (* Winner player 1:no color *)
let win2temp = make_image (rip "images/play2Win") (* Winner player 2:no color *)
let huHi = make_image (rip "images/human") (* Highlight Human player *)
let aiHi = make_image (rip "images/ai") (* Highlight AI player *)
let colorWheel = make_image (rip "images/colorwheel") (* Color Wheel *)
let howTo = make_image (rip "images/howTo") (* How to play Image *)
let menu = make_image (rip "images/menu") (* Main menu Image *)
let img0 = make_image (rip "images/0") (* '0' for walls *)
let img1 = make_image (rip "images/1") (* '1' for walls *)
let img2 = make_image (rip "images/2") (* '2' for walls *)
let img3 = make_image (rip "images/3") (* '3' for walls *)
let img4 = make_image (rip "images/4") (* '4' for walls *)
let img5 = make_image (rip "images/5") (* '5' for walls *)
let img6 = make_image (rip "images/6") (* '6' for walls *)
let img7 = make_image (rip "images/7") (* '7' for walls *)
let img8 = make_image (rip "images/8") (* '8' for walls *)
let img9 = make_image (rip "images/9") (* '9' for walls *)
let img10 = make_image (rip "images/10") (* '10' for walls *)
let enumImg = [img0;img1;img2;img3;img4;img5;img6;img7;img8;img9;img10]
              (* Enum for the numbe of walls images *)

(* Set up defaults for view *)
let save = ref (create_board 9)
let button = ref false (* Monitor button status between windows *)
let instance = ref Blank (* Current board action state *)
let play1ban = ref ban1 (* Banner for player 1 *)
let play2ban = ref ban2 (* Banner for player 2 *)
let win1 = ref win1temp (* Winner display player 1 *)
let win2 = ref win2temp (* Winner display player 2 *)

(* Convert an array y marking to pixel coordinate *)
let yf z =
  let inv1 = (16 - z)/2 in
  let inv = if (16 - z) = -1 then -1 else inv1 in (* compensate for '/' spec *)
  match z mod 2 with
    | 0 -> inv*100
    | _ -> inv*100 + 80

(* Convert an array x marking to pixel coordinates *)
let xf z =
  let inv = z/2 in
  match z mod 2 with
    | 0 -> inv*100
    | _ -> inv*100 + 80

(* draw a player's circle and their pawn cover *)
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
          | Wall -> draw_image wallV (xf (j*2 + 1)) (yf i) (* Vertical walls *)
          | _ -> ()
      done
    else
      for j = 0 to 16 do
        match board.(i).(j) with
          | Wall -> (match j mod 2 with
                      | 0 -> draw_image wallH (xf j) (yf i) (*Horizontal walls*)
                      | _ -> draw_image wallC (xf j) (yf i)) (* Central walls *)
          | _ -> ()
      done
  done

(* draw a ghost wall for the player to see a possible wall on placement, based
   upon how a wall would normally be represented in save *)
let rec draw_ghost (walls:loc list) =
  set_color (rgb 158 132 101);
  match walls with
    | [] -> ()
    | (y, x)::t -> match y mod 2 with
                    | 0 -> fill_rect (xf x) (yf y) 20 80; draw_ghost t
                    | _ -> match x mod 2 with
                            | 0 -> fill_rect (xf x) (yf y) 80 20; draw_ghost t
                            | _ -> fill_rect (xf x) (yf y) 20 20; draw_ghost t

(* Main draw function: ghosts are the walls that are supposed to be draw as a
                        ghost
                       players is a list of the players in the game
                       play is an index for one of the players in players
  Clears graph and then draws board, players, walls, ghosts, the banner,
   current player and player's number of walls, and synchronizes the image
   in that order*)
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


(* Draws main menu screen*)
let rec drawMenu () =
  clear_graph graph;
  draw_image menu 0 0;
  synchronize graph

(* Draws How to Play screen*)
and drawHow () =
  clear_graph graph;
  draw_image howTo 0 0;
  synchronize graph

(* Draws the player customization screen,
    c1 and c2 reprsent the controller of player 1 and player 2 respectively,
    c and c' represent the colors (based on graphics color spec) of player 1
      and play 2 respectively,
    cw is a boolean that indicates whether or not the color wheel should be
    drawn *)
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

(* Iterates to next player, maintaining that player is always an index of
   players *)
and next_player players player =
  let num = List.length players in
  (player + 1) mod num

(* Checks if either player is in a location of their "winning line", returning
   true or false, and then the index of the player that won *)
and checkWin players =
  if (List.nth players 0).pos_y = 16 then (true, 0) else
  if (List.nth players 1).pos_y = 0 then (true, 1) else
  (false, 3)

(* Event handler for the win screen, directs to either
   player customization screen or closes game *)
and waitWin () =
  let event = try wait_next_event [Poll] with
                Graphic_failure _ -> (close_graph graph; exit 0) in
  if (event.button && not(!button)) then
    let posx = event.mouse_x in
    let posy = event.mouse_y in
    if (posx >= 220 && posx <= 420 && posy >= 340 && posy <= 420) then
      playerInit () (* Player customization *)
    else if (posx >= 460 && posx <= 660 && posy >= 340 && posy <= 420) then
      ignore(clear_graph graph; exit 0) (* Exit *)
    else
      button := event.button; waitWin ()
  else
    button := event.button; waitWin ()


(* Draws the win overlay, indicating who has won *)
and drawWin play =
  draw_image win 0 0;
  (match play with
    | 0 -> draw_image (!win1) 252 467
    | _ -> draw_image (!win2) 252 467);
  synchronize graph; button := true; waitWin ()

(* Event handler Main game loop
  Players: list of all the players
  cur_player: index of who's action it currently is *)
and loop (players:player list) (cur_player:int) =
  let win = checkWin players in (* Check if someone has won *)
  (if (fst win) then drawWin (snd win) else ()); (*If win, then do win actions*)
  if (List.nth players cur_player).cont = Human then (* Check if human or ai *)
    let event = try wait_next_event [Poll] with
                Graphic_failure _ -> (close_graph graph; exit 0)in
    let new_player =
      if (event.key = '\027') then (close_graph graph; exit 0)
      else if event.button && not(!button) then (* Check for button press *)
        let modx = event.mouse_x mod 100 in
        let mody = event.mouse_y mod 100 in
        let my = abs(16 - ((event.mouse_y / 100) * 2)) -
                  if mody >= 80 then 1 else 0 in
        let mx = (event.mouse_x / 100) * 2 + if modx >= 80 then 1 else 0 in
        if(modx < 80 && mody < 80) then (* Check if move or wallPlace *)
          let mv = validate_move cur_player (Move(my,mx)) (!save) in
          if mv then (* Accept move *)
            let () = commit_move cur_player (Move(my,mx)) (!save) in
            let play = List.nth players cur_player in
            play.pos_x <- mx; play.pos_y <- my;
            let next = next_player players cur_player in
            instance := Blank; draw [] players next; next
          else (* Decline move attempt *)
            let _ = instance := MoveInvalid; draw [] players cur_player in
            cur_player
        else
          let accept = (* Check for a wall placement *)
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
          if accept (* If accept wall placement *)
            then
              let next = next_player players cur_player in
              instance := Blank;
              draw [] players next; next
            else (* If deny wall placement *)
              let _ = instance := WallInvalid; draw [] players cur_player in
              cur_player
      else (* Check for a ghost wall display *)
        let modx = event.mouse_x mod 100 in
        let mody = event.mouse_y mod 100 in
        let my = abs(16 - ((event.mouse_y / 100) * 2)) -
                  if mody >= 80 then 1 else 0 in
        let mx = (event.mouse_x / 100) * 2 + if modx >= 80 then 1 else 0 in
        let ghosts =
          if (mody >= 80 && modx >= 80) then [] (* center square to deny *)
          else if (mody <= 50 && modx > 80) then (* Down *)
            [(my, mx);(my+1, mx);(my+2, mx)]
          else if (modx <= 50 && mody > 80) then (* Left *)
            [(my, mx);(my, mx-1);(my, mx-2)]
          else if (modx > 80 && mody > 50) then (* Up *)
            [(my, mx);(my-1, mx);(my-2, mx)]
          else if (mody > 80 && modx > 50) then (* Right *)
            [(my, mx);(my, mx+1);(my, mx+2)]
          else [] (* Not a wall location *)
        in
        draw ghosts players cur_player; cur_player (* Display wall *)
    in
    let _ = button := event.button in
    loop players new_player
  else (* AI turn, ask model AI for a next move *)
    let next = next_player players cur_player in
    instance := AiThink; draw [] players cur_player;
    let () = commit_move cur_player (Ai.next_move !save cur_player) !save in
    let play = List.nth players cur_player in
    play.pos_x <- (snd (fst (!save).players.(cur_player)));
    play.pos_y <- (fst (fst (!save).players.(cur_player)));
    instance := Blank; draw [] players next; loop players next


(* Sets up players and initializes the game loop
   c1 and c2 are the controllers of player 1 and player 2 respectively,
   c and c' are the colors of player 1 and player 2 respectively *)
and players (c1:controller) (c2:controller) (c:color) (c':color) =
  button := true;
  play1ban := (make_image (ripColorize "images/player1" c)); (* colorizations *)
  play2ban := (make_image (ripColorize "images/player2" c'));
  win1 := (make_image (ripColorize "images/play1Win" c));
  win2 := (make_image (ripColorize "images/play2Win" c'));
  let pl_1 =
    {cont = c1; color = c; pos_x = 8; pos_y = 0} in
  let pl_2 =
    {cont = c2; color = c'; pos_x = 8; pos_y = 16} in
  loop [pl_1;pl_2] (Random.int 2) (* Random starting player *)

(* Event handler for instructions menu *)
and howToPlayLoop () =
  let event = try wait_next_event [Poll] with
                Graphic_failure _ -> (close_graph graph; exit 0) in
  if (event.key = '\027') then ignore(close_graph graph; exit 0)  else
  if (event.button && not(!button)) then
    let posx = event.mouse_x in
    let posy = event.mouse_y in
    if (posy <= 160) then
      if (posx <= 220) then
        menuInit () (* Main Menu *)
      else if (posx >= 660) then
        playerInit () (* Player customization *)
      else
        howToPlayLoop ()
    else
      howToPlayLoop ()
  else button := event.button; howToPlayLoop ()

(* Initializes instructions Loop *)
and howToPlayLoopInit () =
  button := true; drawHow (); howToPlayLoop ()

(* Event handler for the main menu *)
and menuLoop () =
  let event = try wait_next_event [Poll] with
                Graphic_failure _ -> (close_graph graph; exit 0) in
  if (event.key = '\027') then ignore(close_graph graph; exit 0)  else
  if (event.button && not(!button)) then
    let posx = event.mouse_x in
    let posy = event.mouse_y in
    if (posy >= 145 && posy <= 340) then
      if (posx >= 97 && posx <= 422) then
        playerInit () (* Player customization *)
      else if (posx >= 470 && posx <= 784) then
        howToPlayLoopInit () (* How to play *)
      else
        menuLoop ()
    else
      menuLoop ()
  else button := event.button; menuLoop ()

(* Initilization for menu loop *)
and menuInit () = button := true; drawMenu (); menuLoop ()

(* Initilization for player customzation loop *)
and playerInit () =
  button := true; drawPlayer Human Human red green false;
  save := (create_board 9); playerLoop Human Human red green false 0

(* Handler for player customization
    c1 and c2 are the controller types for player 1 and player 2 respectively,
    c and c' are the colors for player 1 and player 2 respectively
    cw is a boolean indicating whether the color wheel is open or not
    id is the index of player that the color wheel applies to
      *Only matter if cw is true, otherwise it is not used* *)
and playerLoop
      (c1:controller) (c2:controller) (c:color) (c':color) (cw:bool) (id:int) =
  drawPlayer c1 c2 c c' cw;
  let event = try wait_next_event [Poll] with
                Graphic_failure _ -> (close_graph graph; exit 0) in
  if (event.key = '\027') then ignore(close_graph graph; exit 0)  else
  if cw then (* Check if color wheel open *)
    if (event.button && not(!button)) then
      let posx = event.mouse_x in
      let posy = event.mouse_y in
      if ((abs(posx - 440) * abs(posx - 440) +
           abs(posy - 460) * abs(posy - 460)) < 50625) then
        let col = point_color posx posy in
        match id with (* Set picke color to id player *)
          | 0 -> (button := true; playerLoop c1 c2 col c' false 0)
          | _ -> (button := true; playerLoop c1 c2 c col false 1)
      else
        playerLoop c1 c2 c c' cw id
    else
      (button := event.button; playerLoop c1 c2 c c' cw id)
  else if (event.button && not(!button)) then (* Check for mouse button *)
    let posx = event.mouse_x in
    let posy = event.mouse_y in
    if (posy <= 100) then
      if (posx <= 200) then
        menuInit () (* Main menu *)
      else if (posx >= 700) then
        players c1 c2 c c' (* Play game *)
      else
       playerLoop c1 c2 c c' cw id
    else if (posy >= 710 && posy <= 775 && posx >= 358 && posx <= 590) then
      playerLoop Human c2 c c' cw id (* Set c1 to Human *)
    else if (posy >= 692 && posy <= 772 && posx >= 694 && posx <= 782) then
      playerLoop Ai c2 c c' cw id (* Set c1 to AI *)
    else if (posy >= 430 && posy <= 498 && posx >= 368 && posx <= 597) then
      playerLoop c1 Human c c' cw id (* Set c2 to Human *)
    else if (posy >= 415 && posy <= 496 && posx >= 704 && posx <= 793) then
      playerLoop c1 Ai c c' cw id (* Set c2 to AI *)
    else if (posy >= 574 && posy <= 702 && posx >= 436 && posx <= 565) then
      (button := true; playerLoop c1 c2 c c' true 0) (* Open CW for player 1 *)
    else if (posy >= 294 && posy <= 426 && posx >= 436 && posx <= 565) then
      (button := true; playerLoop c1 c2 c c' true 1) (* Open CW for player 2 *)
    else playerLoop c1 c2 c c' cw id
  else button := event.button; playerLoop c1 c2 c c' cw id

(* Begins the program at the main menu*)
let _ = menuInit ()