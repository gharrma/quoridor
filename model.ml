(* POS1 | WAL1 | POS2 | WAL
 * -------------------------
 * WAL2 | EMP0 | WAL3 | EMP0
 * -------------------------
 * POS3 | WAL4 | POS4 | WAL5
 * -------------------------
 * WAL6 | EMP0 | WAL7 | EMP0
 *)

type id = int

type board_object =
  | Wall  | NoWall
  | Space | Player of id

type loc = (int * int)

type t = {
  board : board_object array array;
  players : (loc * int) array
}

type wall = loc list

type move =
  | Move of loc
  | PlaceWall of wall

let print_game t =
  Array.iter (fun row ->
    Array.iter (function
      | NoWall    -> print_string ". "
      | Wall      -> print_string "+ "
      | Space     -> print_string "  "
      | Player id -> print_int id; print_char ' '
    ) row; print_newline()
  ) t.board

let create_board size =
  let rep_size = size * 2 - 1 in
  let p0_pos = ((0, rep_size / 2), 10) in
  let p1_pos = ((rep_size - 1, rep_size / 2), 10) in
  let players = [| p0_pos; p1_pos |] in

  let fill_space y x =
         if (x, y) = fst p0_pos then Player 0
    else if (x, y) = fst p1_pos then Player 1
    else if x mod 2 = 0 && y mod 2 = 0 then Space
    else NoWall in
  let fill_row y = Array.init rep_size (fill_space y) in
  let board = Array.init rep_size fill_row in

  { board; players }

(* Returns whether a location is unacessible, either because it has a wall,
 is not a valid board location or is occupied by a player. *)
let haswall board y x =
  let n = Array.length board.board in
  if(y < 0 || y >= n || x < 0 || x >= n) then true else
  match (board.board).(y).(x) with
  |Wall -> true
  |Player i -> true
  |_ -> false

(* Returns the point in the plane corresponding to the reflection of a over b *)
let reflect a b =
  let (ya, xa) = a in let (yb, xb) = b in (2*yb - ya, 2*xb - xa)

let validate_move player_id move board =
  let ((py, px), nwalls) = (board.players).(player_id) in
  match move with
  |Move(y, x) -> let canmove = begin
    match board.board.(y).(x) with
    |Space -> begin
      if(abs(px - x) + abs(py - y) == 2) then
        not (haswall board ((py + y)/2) ((px + x)/2))
    else if(abs(px - x) + abs(py - y) > 4) then false (* else dist = 4 *)
    else let (my, mx) = ((py + y)/2, (px + x)/2) in
    if (px == x || py == y) then (* jumping *)
      haswall board my mx &&
      not (haswall board ((my + py)/2) ((mx + px)/2)) &&
      not (haswall board ((my + y)/2) ((mx + x)/2))
    else (* diagonal move *)
    let can1 = not (haswall board py mx) &&
               not (haswall board my x) &&
               ((let (wy, wx) = reflect (py, mx) (py, x) in haswall board wy wx)
               ||
               (let (qy, qx) = reflect (py, px) (py, x) in haswall board qy qx))
               in
    let can2 = not (haswall board my px) &&
               not (haswall board y mx) &&
               ((let (wy, wx) = reflect (my, px) (y, px) in haswall board wy wx)
               ||
               (let (qy, qx) = reflect (py, px) (y, px) in haswall board qy qx))
               in
    match (board.board.(py).(x), board.board.(y).(px)) with
      |(Player a, Player b) -> can1 || can2
      |(Player a, _) -> can1
      |(_, Player b) -> can2
      |_ -> false
    end
    |_ -> false (* cannot move somewhere not a space *)
  end in if (canmove) then begin (* update the board *)
    (board.board.(py).(px) <- Space);
    (board.board.(y).(x) <- Player player_id);
    (board.players.(player_id) <- ((y, x), nwalls));
    (canmove, board) end else (false, board)
  |PlaceWall wlist -> let canmove = begin
    if (nwalls == 0) then false else
    let rec canplace = function
    |[] -> true
    |(y, x)::tl -> not (haswall board y x) && canplace tl in
    if (not (canplace wlist)) then false else
    let n = (Array.length board.board + 1)/2 in
    let mark = ref (Array.init n (fun x -> Array.init n (fun x -> false))) in
    let top = ref false in let bot = ref false in
    let dirs = [|(0, 2);(2, 0);(-2, 0);(0,-2)|] in
    let rec visit mark top bot (y, x) =
      if (x < 0 || y < 0 || x >= 2*n || y >= 2*n || !mark.(y/2).(x/2))
       then ()
      else begin !mark.(y/2).(x/2) <- true;
      (if (y == 0) then top := true else
       if (y == 2*n - 2) then bot := true else ());
      for i = 0 to 3 do let (dy, dx) = dirs.(i) in
       if(not (haswall board (y + dy/2) (x + dx/2) ||
       List.mem (y + dy/2, x + dx/2) wlist))
       then visit mark top bot (y + dy, x + dx) else () done end
    in (visit mark top bot (py, px));
    if player_id = 0 then !bot else !top
    end in if (canmove) then begin (* update the board *)
    let rec updatewalls = function
    |[] -> ()
    |(y, x)::tl -> (board.board.(y).(x) <- Wall); updatewalls tl in
    (updatewalls wlist);
    (board.players.(player_id) <- ((py, px), nwalls - 1));
    (canmove, board) end else (false, board)

let ai_move board player_id =
  failwith "TODO"

let board_from_file s =
  failwith "TODO"

(*let _ = print_game (create_board 7)*)
