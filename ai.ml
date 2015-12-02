open Model

(* Returns the number of moves required to win for a given player. *)
let dist_to_win board player_id =
  let (locn, _) = (board.players).(player_id) in
  let q = Queue.create() in
  let vis = Hashtbl.create 100 in
  let dist = Hashtbl.create 100 in
  Queue.push locn q;
  Hashtbl.add vis locn true;
  let cur_nodes = ref 1 in
  let next_nodes = ref 0 in
  let depth = ref 0 in
  while (not (Queue.is_empty q)) do
    (* Printf.printf "queue length: %d\n" (Queue.length q);
    Printf.printf "hashtable #keys: %d\n" (Hashtbl.length vis); *)
	  let (py, px) = Queue.pop q in
	  Hashtbl.add dist (py, px) !depth;
	  decr cur_nodes;
	  if !cur_nodes = 0 then
		  incr depth;
		  cur_nodes := !next_nodes;
		  next_nodes := 0;
	  let chk_neighbor (py, px) (qy, qx) =
		  let max_ordinate = (2*board.size - 2) in
		  if qx >= 0 && qy >= 0 && qx <= max_ordinate && qy <= max_ordinate &&
  				not ((board.board).((py+qy)/2).((px+qx)/2) = Wall) &&
  				not (Hashtbl.mem vis (qy, qx)) then
			  incr next_nodes;
			  Queue.push (qy, qx) q;
        Hashtbl.add vis (qy, qx) true;
	  in
	  List.iter (chk_neighbor (py, px)) [(py-2, px); (py+2, px); (py, px-2); (py, px+2)]
  done;
  let dist_to dest_locn =
	  try Hashtbl.find dist dest_locn with
	  | Not_found -> max_int
  in
  let rec build_inc_lst n m = (* [(n,m), (n-2,m), (n-4,m), ..., (0,m)] *)
	if n >= 0 then (n,m)::(build_inc_lst (n-2) m) else [] in
  let winning_locns = if player_id = 0 then
  build_inc_lst (2*board.size - 2) (2*board.size - 2)
  else build_inc_lst (2*board.size - 2) 0 in
  List.fold_left min 0 (List.map dist_to winning_locns)


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
  let rec build_inc_lst n = (* 0, 1, 2, ..., size-1 *)
	if n < board.size then n::(build_inc_lst (n+1)) else [] in
  let all  = build_inc_lst 0 in
  let even = List.map (fun x -> 2 * x)     all in
  let odd  = List.map (fun x -> 2 * x + 1) all in
  let wall_placements =
	List.flatten (
	  List.map (fun x ->
		if x mod 2 = 1 then
		  List.map (fun y -> PlaceWall [(y,x); (y+2,x)]) even
		else
		  List.map (fun y -> PlaceWall [(y,x); (y,x+2)]) odd
	  ) all
	) in

  (* Filter out invalid moves *)
  List.filter
  (fun m -> validate_move player_id m board)
  (moves @ wall_placements)


(* Returns a copy of the game *)
let copy game =
  {game with board = Array.init (2*game.size - 1)
                                (fun i -> Array.copy game.board.(i))}

(* Returns a list of movements that achieve the maximal value of minimal value
the opponent can force the player into, where the value of a position is defined
as the difference between the distances of the players to their respective edge
 of the board *)
let minimax game player_id =
  let op_id = 1 - player_id in
  let pml = get_valid_moves game player_id in
	let rec best game oid pid = function
	  |[] -> (-100, [])
	  |pm::ptl -> let pb = copy game in (commit_move pid pm pb);
				  let oml = get_valid_moves pb oid in
				  let rec worse b = function
					|[] -> 100
					|om::otl -> let ob = copy b in (commit_move oid om ob);
					  let d = dist_to_win ob oid - dist_to_win ob pid in
					  let w = worse b otl in if(d < w) then d else w
				  in let w = worse pb oml in
				  let (prevbest, bestmoves) = best game oid pid ptl in
				  if (w == prevbest) then (w, pm::bestmoves) else
				  if (w > prevbest) then (w, [pm]) else (prevbest, bestmoves)
  in snd (best game op_id player_id pml)

let next_move game player_id =
  let moves = minimax game player_id in
  List.nth moves (Random.int (List.length moves))
