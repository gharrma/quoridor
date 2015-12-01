include Model

(* Returns the number of moves required to win for a given player. *)
let dist_to_win game player_id =
  failwith "TODO"

(* Returns a list of all possible moves that a given player can make. *)
let get_valid_moves game player_id = 
  failwith "TODO"

let minimax game player_id =
  failwith "TODO"

let next_move game player_id =
  let pos = game.players.(player_id) in
  Move pos
  (* TODO: thanksgiving *)
