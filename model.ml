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

let validate_move move board player_id =
  failwith "TODO"

let ai_move board player_id =
  failwith "TODO"

let board_from_file s =
  failwith "TODO"
