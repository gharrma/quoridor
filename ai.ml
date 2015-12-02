open Model

(* Returns the number of moves required to win for a given player. *)
let dist_to_win board player_id =
  let (locn, _) = (board.players).(player_id) in let (iy, ix) = locn in
  let q = Queue.create() in
  let n = board.size in
  let dist = Array.init n (fun x -> Array.init n (fun x -> -1)) in
  Queue.push (iy/2, ix/2) q;
  (dist.(iy/2).(ix/2) <- 0);
  while (not (Queue.is_empty q)) do
	  let (py, px) = Queue.pop q in
	  let chk_neighbor (py, px) (qy, qx) =
		  if qx >= 0 && qy >= 0 && qx <= n - 1 && qy <= n - 1 &&
  				not ((board.board).(py+qy).(px+qx) = Wall) &&
  				dist.(qy).(qx) = -1 then begin
			  Queue.push (qy, qx) q;
        (dist.(qy).(qx) <- dist.(py).(px) + 1);
      end
	  in List.iter (chk_neighbor (py, px))
    [(py-1, px); (py+1, px); (py, px-1); (py, px+1)]
  done;
  let dto y x = if (dist.(y).(x) > 0) then dist.(y).(x) else max_int in
  let rec ans a b = if (b = 0) then dto a 0 else min (dto a b) (ans a (b-1)) in
	if player_id = 0 then ans (n-1) (n-1) else ans 0 (n-1)

(* Returns a list of all possible moves that a given player can make. *)
let get_valid_moves board player_id =
  let ((py, px), nwalls) = (board.players).(player_id) in
  let ((oy, ox), owalls) = (board.players).(1 - player_id) in

  (* possible position moves (including jumps) *)
  let adj_pos (y,x) = [(y+2,x); (y-2,x); (y,x+2); (y,x-2)] in
  let player_adj = adj_pos (py,px) in
  let opponent_adj = adj_pos (oy,ox) in
  let moves = List.map (fun m -> Move m) (player_adj @ opponent_adj) in

  (* possible wall placements *)
  (* only consider down and right, to avoid considering a pair of walls twice *)
  let rec build_inc_lst n thresh = (* 0, 1, 2, ..., thresh-1 *)
	if n < thresh then n::(build_inc_lst (n+1) thresh) else [] in
  let all  = build_inc_lst 0 board.size in
  let even = List.map (fun x -> 2 * x)     all in
  let odd  = List.map (fun x -> 2 * x + 1) all in
  let all_full = build_inc_lst 0 (board.size * 2 - 2) in
  let wall_placements =
	List.flatten (
	  List.map (fun x ->
		if x mod 2 = 1 then
		  List.map (fun y -> PlaceWall [(y,x); (y+1,x); (y+2,x)]) even
		else
		  List.map (fun y -> PlaceWall [(y,x); (y,x+1); (y,x+2)]) odd
	  ) all_full
	) in

  (* Filter out invalid moves *)
  List.filter
  (fun m -> validate_move player_id m board)
  (moves @ wall_placements)

(* Returns a list of movements that achieve the maximal value of minimal value
the opponent can force the player into, where the value of a position is defined
as the difference between the distances of the players to their respective edge
 of the board *)
let minimax game player_id =
  let op_id = 1 - player_id in
  let pml = get_valid_moves game player_id in
  if List.length pml = 0 then Printf.printf "Oopsy daisy!\n%!";
  Printf.printf "now pscore = %d, enemy = %d\n%!"
            (dist_to_win game op_id) (dist_to_win game player_id);
  let (ploc, oloc) = (fst game.players.(player_id), fst game.players.(op_id)) in
	let rec best game oid pid = function
	  |[] -> (-max_int, [])
	  |pm::ptl -> (commit_move pid pm game);
				  let oml = get_valid_moves game oid in
				  let rec worse b = function
					|[] -> max_int
					|om::otl -> (commit_move oid om b);
            let (x, y) = (dist_to_win b pid, dist_to_win b oid) in
            let a = if (y <= 8) then y*(16-y) else 4*y + 32 in
            let d = a - 8*x in (undo oid om b oloc); min d (worse b otl)
				  in let w = worse game oml in (undo pid pm game ploc);
				  let (prevbest, bestmoves) = best game oid pid ptl in
				  if (w = prevbest) then (w, pm::bestmoves) else
				  if (w > prevbest) then (w, [pm]) else (prevbest, bestmoves)
  in snd (best game op_id player_id pml)

let next_move game player_id =
  let (ploc, nwalls) = game.players.(player_id) in
  let moves = if (nwalls = 0) then
  let pml = get_valid_moves game player_id in
  let rec closer game pid = function
    |[] -> (max_int, [])
    |pm::ptl -> (commit_move pid pm game);
                let d = dist_to_win game pid in
                (undo pid pm game ploc);
                let (prevbest, bestmoves) = closer game pid ptl in
                if (d = prevbest) then (d, pm::bestmoves) else
                if (d < prevbest) then (d, [pm]) else (prevbest, bestmoves)
  in snd (closer game player_id pml) else minimax game player_id in
  Printf.printf "Have %d moves available..\n%!" (List.length moves);
  let mv = List.nth moves (Random.int (List.length moves)) in
  match mv with
    |Move(x, y) -> (Printf.printf "let's move  to (%d,%d)\n%!" x y); mv
    |_ -> (); mv
