include Model

let next_move game player_id =
  let pos = game.players.(player_id) in
  Move pos
  (* TODO: thanksgiving *)
