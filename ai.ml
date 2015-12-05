open Model

let print_game t =
  Array.iter (fun row ->
    Array.iter (function
      | NoWall    -> print_string ". "
      | Wall      -> print_string "+ "
      | Space     -> print_string "  "
      | Player id -> print_int id; print_char ' '
    ) row; print_newline()
  ) t.board

(* Returns the number of moves required to win for a given player.
   Uses breadth-first search. *)
let dist_to_win board player_id =
  let ((iy, ix), _) = (board.players).(player_id) in
  let q = Queue.create() in
  let n = board.size in
  let dist = Array.init n (fun x -> Array.init n (fun x -> -1)) in
  Queue.push (iy/2, ix/2) q;
  dist.(iy/2).(ix/2) <- 0;
  while not (Queue.is_empty q) do
    let (py, px) = Queue.pop q in
    let chk_neighbor (py, px) (qy, qx) =
      if qx >= 0 && qy >= 0 && qx <= n - 1 && qy <= n - 1 &&
          not ((board.board).(py+qy).(px+qx) = Wall) &&
          dist.(qy).(qx) = -1 then begin
        Queue.push (qy, qx) q;
        dist.(qy).(qx) <- dist.(py).(px) + 1;
      end
    in List.iter (chk_neighbor (py, px))
    [(py-1, px); (py+1, px); (py, px-1); (py, px+1)]
  done;
  let dto y x = if dist.(y).(x) >= 0 then dist.(y).(x) else max_int in
  let rec ans a b = if b = 0 then dto a 0 else min (dto a b) (ans a (b-1)) in
  if player_id = 0 then ans (n-1) (n-1) 
  else ans 0 (n-1)

(* Returns list consisting of a possible path to victory (no jumping),
   where the head is the current position of the player
   and the last element is a winning position. *)
let rec path_to_win game player_id =
  let op_id = 1 - player_id in
  let (py, px) = fst game.players.(player_id) in
  let (oy, ox) = fst game.players.(op_id) in
  if dist_to_win game player_id = 0 then [(py, px)] else
  let n = game.size in
  let valid_adj (dy, dx) =
    let (my, mx) = (py + dy, px + dx) in
    mx >= 0 && my >= 0 && mx <= 2*n-2 && my <= 2*n-2
    && not (game.board.(py + dy/2).(px + dx/2) = Wall)
  in
  let mlist = List.filter valid_adj [(0,2); (0,-2); (2,0); (-2,0)] in
  let rec closest = function
    |[] -> (Move(py, px), max_int)
    |(dy, dx)::ptl -> let m = Move(py+dy, px+dx) in
                      commit_move player_id m game;
                      let d = dist_to_win game player_id in
                      undo player_id m game (py, px);
                      game.board.(oy).(ox) <- Player op_id;
                      let (pm, dis) = closest ptl in
                      if (d < dis) then (m, d) else (pm, dis)
  in
  let (pm, dis) = closest mlist in
  commit_move player_id pm game;
  let path = path_to_win game player_id in
  undo player_id pm game (py, px);
  game.board.(oy).(ox) <- Player op_id;
  (py, px)::path

(* Return the given path represented as an array *)
let pathtoarray path n =
  let ans = Array.init (2*n-1) (fun y -> Array.init (2*n-1) (fun x -> false)) in
  let rec fill = function
    |[] -> ()
    |[(y, x)] -> ans.(y).(x) <- true
    |(ay, ax)::(by, bx)::c -> ans.(ay).(ax) <- true;
                              ans.(by).(bx) <- true;
                              ans.((ay+by)/2).((ax+bx)/2) <- true;
                              fill ((by,bx)::c)
  in fill path; ans

(* Checks whether the move consists of a wall cutting the given path. *)
let cutspath move patharray =
  match move with
  | Move(y, x) -> false
  | PlaceWall wlist -> 
    let rec checkwalls = function
      | [] -> false
      | (wy, wx)::wtl -> patharray.(wy).(wx) || checkwalls wtl
    in checkwalls wlist

(* Optimizes dist_to_win by avoiding unnecessary recomputations. *)
let dist_given_path game player_id prev_dist prev_pos path =
  if fst game.players.(player_id) <> prev_pos then
   dist_to_win game player_id
  else 
    let rec intersect = function
      | [] -> 0
      | [pos] -> prev_dist
      | (ay, ax)::(by, bx)::c -> if game.board.((ay+by)/2).((ax+bx)/2) = Wall 
                                 then dist_to_win game player_id
                                 else intersect ((by, bx)::c)
    in intersect path

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
  let all = build_inc_lst 0 (board.size - 1) in
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
    )
  in

  (* Filter out invalid moves *)
  let parray = pathtoarray (path_to_win board player_id) board.size in
  let oarray = pathtoarray (path_to_win board (1 - player_id)) board.size in

  List.filter (fun m ->
    match m with
    | Move(y, x) -> validate_move player_id m board
    | PlaceWall wlist -> begin
      if nwalls = 0 then false else
      let rec canplace = function
        | [] -> true
        | (y, x)::tl -> board.board.(y).(x) <> Wall && canplace tl
      in
      if not (canplace wlist) then false else
      if cutspath m parray || cutspath m oarray then
        validate_move player_id m board
      else true
    end
  ) (moves @ wall_placements)

let rec printpath = function
  | [] -> ()
  | [(y, x)] -> Printf.printf "(%d, %d)\n%!" x y
  | (y, x)::tl -> (Printf.printf "(%d, %d)--" x y); printpath tl

let printparray patharray =
  Array.iter(fun y ->
    Array.iter(fun x ->
      Printf.printf "%d " (if x then 1 else 0)
    ) y; print_newline()
  ) patharray

let ismove = function
  | Move(x, y) -> true
  | _ -> false

(* Quantify the state of the board, with a higher number representing a game
   state more favorable to the given player. *)
let heuristic game player_id pd od ppos opos ppath opath =
  let pdist  = dist_given_path game player_id pd ppos ppath in
  let pwalls = snd game.players.(player_id) in
  let odist = dist_given_path game (1 - player_id) od opos opath in
  let owalls = snd game.players.(1 - player_id) in
  let square n = n * n in
  square (100 - pdist) - square (10 - pwalls)
  - (square (100 - odist) + square (10 - owalls))

(* Returns a list of movements that maximizes the given player's advantage in
   the game (based on the heuristic function above). Uses the minimax algorithm
   (https://en.wikipedia.org/wiki/Minimax) with alpha-beta pruning
   (https://en.wikipedia.org/wiki/Alpha–beta_pruning). *)
let minimax game player_id =
  let pprev_loc = fst game.players.(player_id) in
  let oprev_loc = fst game.players.(1 - player_id) in
  let ppath = path_to_win game player_id in
  let opath = path_to_win game (1 - player_id) in
  let pdist = List.length ppath - 1 in
  let odist = List.length opath - 1 in
  let rec alphabeta game depth alpha beta maximizing = (* returns (score, move) *)
    if depth = 0 then
      (heuristic game player_id pdist odist pprev_loc oprev_loc ppath opath,
       None)
    else if maximizing = true then
      let moves = get_valid_moves game player_id in
      let find_best (best, best_moves, alpha) move =
        if beta <= alpha then
          (best, best_moves, alpha)
        else
          let () = commit_move player_id move game in
          let (score,_) = 
            alphabeta game (depth - 1) alpha beta (not maximizing) in
          let () = undo player_id move game pprev_loc in
          let (best, best_moves) =
            if score = best then (score, move::best_moves)
            else if score > best then (score, [move])
            else (best, best_moves) in
          let alpha = max alpha best in
          (best, best_moves, alpha)
      in
      let start_acc = (min_int, [], min_int) in
      let (best, best_moves, _) = List.fold_left find_best start_acc moves in
      (best, Some best_moves)
    else (* maximizing = false *)
      let moves = get_valid_moves game (1 - player_id) in
      let find_best (best, best_moves, beta) move =
        if beta <= alpha then
          (best, best_moves, beta)
        else
          let () = commit_move (1 - player_id) move game in
          let (score,_) =
            alphabeta game (depth - 1) alpha beta (not maximizing) in
          let () = undo (1 - player_id) move game oprev_loc in
          let (best, best_moves) =
            if score = best then (score, move::best_moves)
            else if score < best then (score, [move])
            else (best, best_moves) in
          let beta = min beta best in
          (best, best_moves, beta)
      in
      let start_acc = (max_int, [], max_int) in
      let (best, best_moves, _) = List.fold_left find_best start_acc moves in
      (best, Some best_moves)
  in
  let minimax_depth = 2 in
  match alphabeta game minimax_depth min_int max_int true with
  | (_,Some best_moves) -> best_moves
  | _ -> failwith "impossible"

let next_move game player_id =
  let t = Sys.time() in
  let ans =
    let (ploc, nwalls) = game.players.(player_id) in
    let moves =
      if (nwalls > 0) then minimax game player_id
      else
        let pml = get_valid_moves game player_id in
        let rec closer game pid = function
          |[] -> (max_int, [])
          |pm::ptl -> commit_move pid pm game;
                      let d = dist_to_win game pid in
                      undo pid pm game ploc;
                      let (prevbest, bestmoves) = closer game pid ptl in
                      if d = prevbest then d, pm::bestmoves 
                      else if d < prevbest then (d, [pm]) 
                      else (prevbest, bestmoves)
        in
        snd (closer game player_id pml)
    in
    Printf.printf "AI has %d moves available\n%!" (List.length moves);
    List.nth moves (Random.int (List.length moves))
  in 
  Printf.printf "AI thinking time: %fs\n%!" (Sys.time() -. t);
  ans
