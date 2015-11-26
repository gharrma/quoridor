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
  players : loc array
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

  let p0_pos = (rep_size / 2, 0) in
  let p1_pos = (rep_size / 2, rep_size - 1) in
  let players = [| p0_pos; p1_pos |] in

  let fill_space y x =
         if (x, y) = p0_pos then Player 0
    else if (x, y) = p1_pos then Player 1
    else if x mod 2 = 0 && y mod 2 = 0 then Space
    else NoWall in
  let fill_row y = Array.init rep_size (fill_space y) in
  let board = Array.init rep_size fill_row in

  { board; players }

let haswall board y x =
  match board.(y).(x) with
  |Wall -> true
  |_ -> false

let validate_move player_id move board =
  let (py, px) = (board.players).(player_id) in
  match move with
  |Move(y, x) -> begin
    match board.(y).(x) with
    |Space -> begin
      if(abs(px - x) + abs(py - y) == 2) then
        not haswall board ((py + y)/2) ((px + x)/2)
    else if(abs(px - x) + abs(py - y) > 4) then false (* else dist = 4 *)
    else let (my, mx) = ((py + y)/2, (px + x)/2) in
    if (px == x || py == y) then
    match board.(my).(mx) with
      |Player -> not haswall board ((my + py)/2) ((mx + px)/2) and
                 not haswall board ((my + y)/2) ((mx + x)/2)
      |_ -> false
    else (* diagonal move *)
    let can1 = not haswall board py mx and
               not haswall board my x in
    let can2 = not haswall board my px and
               not haswall board y mx in
    match (board.(py).(x), board.(y).(px)) with
      |(Player, Player) -> can1 || can2
      |(Player, _) -> can1
      |(_, Player) -> can2
      |_ -> false
    end
    |_ -> false (* cannot move somewhere not a space *)
  end
  |PlaceWall wlist -> (* after thanksgiving dinner :) *)

let ai_move board player_id =
  failwith "TODO"

let board_from_file s =
  failwith "TODO"

let _ = print_game (create_board 7)
