(** Returns the move that the AI determines to be best given the current
    game state. *)
val save_game : Model.t -> unit

(* Load a game state from a JSON file. *)
val load_game : string -> Model.t
