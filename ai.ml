open Model

(* Returns the number of moves required to win for a given player. *)
let dist_to_win board player_id =
  let (locn, _) = (board.players).(player_id) in
  let q = Queue.create() in
  let dist = Hashtbl.create 100 in
  Queue.push locn q;
  Hashtbl.add dist locn 0;
  while (not (Queue.is_empty q)) do
	  let (py, px) = Queue.pop q in
	  let chk_neighbor (py, px) (qy, qx) =
		  let max_ordinate = (2*board.size - 2) in
		  if qx >= 0 && qy >= 0 && qx <= max_ordinate && qy <= max_ordinate &&
  				not ((board.board).((py+qy)/2).((px+qx)/2) = Wall) &&
  				not (Hashtbl.mem dist (qy, qx)) then begin
			  Queue.push (qy, qx) q;
        Hashtbl.add dist (qy, qx) ((Hashtbl.find dist (py, px)) + 1);
      end
	  in
	  List.iter (chk_neighbor (py, px)) [(py-2, px); (py+2, px); (py, px-2); (py, px+2)]
  done;
  let dist_to dest_locn =
	  try Hashtbl.find dist dest_locn with
	  | Not_found -> max_int
  in
  let rec build_inc_lst n m = (* [(n,m), (n,m-2), (n,m-4), ..., (n,0)] *)
	if m >= 0 then (n,m)::(build_inc_lst n (m-2)) else [] in
  let winning_locns = if player_id = 0 then
    build_inc_lst (2*board.size - 2) (2*board.size - 2)
  else build_inc_lst 0 (2*board.size - 2) in
  let res = List.fold_left min max_int (List.map dist_to winning_locns) in
  res


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
