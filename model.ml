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
  size : int;
  board : board_object array array;
  players : (loc * int) array
}

type wall = loc list

type move =
  | Move of loc
  | PlaceWall of wall

(* Prints an ASCI version of the game board. *)
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
         if (y, x) = fst p0_pos then Player 0
    else if (y, x) = fst p1_pos then Player 1
    else if x mod 2 = 0 && y mod 2 = 0 then Space
    else NoWall in
  let fill_row y = Array.init rep_size (fill_space y) in
  let board = Array.init rep_size fill_row in
  { size; board; players }

(* Returns whether a location is unacessible, either because it has a wall,
   is not a valid board location or is occupied by a player. *)
let haswall board y x =
  let n = Array.length board.board in
  if y < 0 || y >= n || x < 0 || x >= n then true
  else
    match (board.board).(y).(x) with
    | Wall -> true
    | Player _ -> true
    | _ -> false

let commit_move player_id move board =
  let ((py, px), nwalls) = (board.players).(player_id) in
  match move with
  | Move(y, x) ->
    board.board.(py).(px) <- Space;
    board.board.(y).(x) <- Player player_id;
    board.players.(player_id) <- ((y, x), nwalls)
  | PlaceWall wlist ->
    let rec updatewalls = function
      | [] -> ()
      | (y, x)::tl ->
        (board.board.(y).(x) <- Wall); updatewalls tl
    in 
    updatewalls wlist;
    (board.players.(player_id) <- ((py, px), nwalls - 1))

let undo player_id move board prevloc =
  let (_, nwalls) = (board.players).(player_id) in
  let (py, px) = prevloc in
  match move with
  | Move(y, x) ->
    board.board.(py).(px) <- Player player_id;
    board.board.(y).(x) <- Space;
    board.players.(player_id) <- ((py, px), nwalls)
  | PlaceWall wlist ->
    let rec destroy = function
    | [] -> ()
    | (y, x)::tl ->
      (board.board.(y).(x) <- NoWall); destroy tl
    in 
    destroy wlist;
    (board.players.(player_id) <- ((py, px), nwalls + 1))


(* Returns the point in the plane corresponding to the reflection of a over b *)
let reflect a b =
  let (ya, xa) = a in
  let (yb, xb) = b in
  (2*yb - ya, 2*xb - xa)

let validate_move player_id move board =
  let ((py, px), nwalls) = (board.players).(player_id) in
  match move with
  | Move(y, x) -> 
    if haswall board y x then 
      false
    else if abs(px - x) + abs(py - y) = 2 then
      not (haswall board ((py + y)/2) ((px + x)/2))
    else if abs(px - x) + abs(py - y) > 4 then
      false
    else (* dist = 4 *)
      let (my, mx) = ((py + y)/2, (px + x)/2) in
      if (px = x || py = y) then (* jumping *)
        haswall board my mx &&
        not (haswall board ((my + py)/2) ((mx + px)/2)) &&
        not (haswall board ((my + y)/2) ((mx + x)/2))
      else (* diagonal move *)
        let can1 =
          let (wy, wx) = reflect (py, mx) (py, x) in
          let (qy, qx) = reflect (py, px) (py, x) in
          not (haswall board py mx) &&
          not (haswall board my x) &&
          (haswall board wy wx || haswall board qy qx)
        in
        let can2 =
          let (wy, wx) = reflect (my, px) (y, px) in
          let (qy, qx) = reflect (py, px) (y, px) in
          not (haswall board my px) &&
          not (haswall board y mx) &&
          (haswall board wy wx || haswall board qy qx)
        in
        begin match board.board.(py).(x), board.board.(y).(px) with
          | Player a, Player b -> can1 || can2
          | Player a, _        -> can1
          | _       , Player b -> can2
          | _                  -> false
        end
  | PlaceWall wlist -> begin
    if (nwalls = 0) then false else
    let rec canplace = function
      |[] -> true
      |(y, x)::tl -> not (haswall board y x) && canplace tl
    in
    if not (canplace wlist) then false
    else begin
      (commit_move player_id move board);
      let n = board.size in
      let mark = Array.init n (fun x -> Array.init n (fun x -> false)) in
      let found = ref false in
      let dirs = [|(-2, 0);(2, 0);(0, -2);(0, 2)|] in
      let rec visit mark found player_id (y, x) =
        if x < 0 || y < 0 || x >= 2*n || y >= 2*n || mark.(y/2).(x/2)
          then ()
        else begin 
          mark.(y/2).(x/2) <- true;
          if player_id = 0 && y = 2*n - 2 then found := true else
          if player_id = 1 && y = 0 then found := true else
          for i = 0 to 3 do 
            let (dy, dx) = dirs.(i) in
            if not (haswall board (y + dy/2) (x + dx/2))
              then visit mark found player_id (y + dy, x + dx)
              else ()
          done
        end
      in 
      visit mark found 0 (fst board.players.(0));
      let ans0 = !found in
      found := false;
      Array.iter (fun row -> Array.fill row 0 n false) mark;
      visit mark found 1 (fst board.players.(1));
      undo player_id move board (py, px);
      ans0 && !found
    end
    end
