(** [next_move game player_id] returns the move that the AI determines to be
    best given the current game state. *)
val next_move : Model.t -> Model.id -> Model.move
