(*
 * POS1 | WAL1 | POS2 | WAL
 * -------------------------
 * WAL2 | EMP0 | WAL3 | EMP0
 * -------------------------
 * POS3 | WAL4 | POS4 | WAL5
 * -------------------------
 * WAL6 | EMP0 | WAL7 | EMP0
 *)

type board_object =
  | Wall  | NoWall
  | Space | Player

type t = {
  board : board_object array array
}

(** A position type for a location on board *)
type loc = (int * int)

(** A position type for a wall (list of segment coordinates) *)
type wall = loc list

(* Represents a single move that a player is choosing to make *)
type move =
  | Move of loc
  | PlaceWall of wall

(** Create empty board state that is n * n in size (Square board) *)
let create_board size =
  let fill_space y x =
    (* TODO: players *)
    if x mod 2 = 0 && y mod 2 = 0 then Space
    else NoWall
  in
  let rep_size = size * 2 - 1 in
  let fill_row y = Array.init rep_size (fill_space y) in
  { board = Array.init rep_size fill_row }

(** Create a board from a JSON game save. *)
let create_board_from_file s =
  failwith "TODO"

(** Validate a movement or wall placement by a player, returns if move is 
  * possible and the resulting board state *)
let validate_move move board =
  failwith "TODO"

(** Request a move from the AI based upon board state, return updated board *)
let request_ai_move board =
  failwith "TODO"
