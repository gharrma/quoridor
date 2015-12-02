(** [next_move game player_id] returns the move that the AI determines to be
    best given the current game state. *)
val next_move : Model.t -> Model.id -> Model.move


(* [distance_to_pos board player_id dest_locn]
 * Returns the distance of player with specified id fron the dest_locn according
 * to the current board configuration. Returns -1 if des_locn cannot be reached.
 *)
val distance_to_pos: Model.t -> Model.id -> Model.loc -> int
