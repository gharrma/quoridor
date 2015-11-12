(** A game state.
  * Walls for an 8x8 board would only be 7x7 (in betweeen spaces) so location
  * for top left wall layout:
  * POS1 | WAL1 | POS2 | WAL
  * -------------------------
  * WAL2 | EMP0 | WAL3 | EMP0
  * -------------------------
  * POS3 | WAL4 | POS4 | WAL5
  * -------------------------
  * WAL6 | EMP0 | WAL7 | EMP0
  *
  * With POS repesenting a position on board, WAL representing a wall and
  * EMP representing an empty space that walls can pass through
  * Coords for WAL 1 -> (1,0)
  * Coords for WAL 2 -> (0,1)
  * Coords for WAL 3 -> (2,1)
  *
  * Coords for POS 1 -> (0,0)
  * Coords for POS 2 -> (2,0)
  * Coords for POS 3 -> (2,0)
  *)
type t

type board_object =
  | Wall  | NoWall
  | Space | Player

(** A position type for a location on board *)
type loc = (int * int)

(** A position type for a wall (list of segment coordinates) *)
type wall_loc = loc list

type move =
  | Move of loc
  | PlaceWall of wall_loc

(** Create empty board state that is n * n in size (Square board) *)
val create_board : int -> t

(** Validate a movement or wall placement by a player, returns if move is 
  * possible and the resulting board state *)
val validate_move : move -> t -> bool * t

(** Request a move from the AI based upon board state *)
val request_move : t -> t
