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

(* Returns the number of moves required to win for a given player. *)
let dist_to_win board player_id =
  let ((iy, ix), _) = (board.players).(player_id) in
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
  let dto y x = if (dist.(y).(x) >= 0) then dist.(y).(x) else max_int in
  let rec ans a b = if (b = 0) then dto a 0 else min (dto a b) (ans a (b-1)) in
  let dwin = if player_id = 0 then ans (n-1) (n-1) else ans 0 (n-1) in
  if (dwin > 1000) then ((print_game board); (Printf.printf "(%d, %d)\n%!" iy ix); dwin) else dwin

(* Returns list consisting of a possible path to victory (no jumping),
  where the head is the current position of the player
  and the last element is a winning position. *)
let rec path_to_win game player_id =
  let (py, px) = fst game.players.(player_id) in let op_id = 1 - player_id in
  let (oy, ox) = fst game.players.(op_id) in
  if(dist_to_win game player_id = 0) then [(py, px)] else let n = game.size in
  let mlist = List.filter (fun (dy, dx) ->
                           let (my, mx) = (py + dy, px + dx) in
                           mx >= 0 && my >= 0 && mx <= 2*n-2 && my <= 2*n-2 &&
                           not (game.board.(py + dy/2).(px + dx/2) = Wall))
                          [(0,2); (0,-2); (2,0); (-2,0)] in
  let rec closest = function
    |[] -> (Move(py, px), max_int)
    |(dy, dx)::ptl -> let m = Move(py+dy, px+dx) in
                      (commit_move player_id m game);
                      let d = dist_to_win game player_id in
                      (undo player_id m game (py, px));
                      (game.board.(oy).(ox) <- Player op_id);
                      let (pm, dis) = closest ptl in if (d < dis) then (m, d)
                      else (pm, dis)
  in let (pm, dis) = closest mlist in (commit_move player_id pm game);
  let path = path_to_win game player_id in
  (undo player_id pm game (py, px));
  (game.board.(oy).(ox) <- Player op_id);
  (py, px)::path

let pathtoarray path n =
  let ans = Array.init (2*n-1) (fun y -> Array.init (2*n-1) (fun x -> false)) in
  let rec fill = function
    |[] -> ()
    |[(y, x)] -> (ans.(y).(x) <- true);
    |(ay, ax)::(by, bx)::c -> (ans.(ay).(ax) <- true); (ans.(by).(bx) <- true);
                              (ans.((ay+by)/2).((ax+bx)/2) <- true);
                              fill ((by,bx)::c)
  in (fill path); ans

(* Checks whether the move consists of a wall cutting the given path. *)
let cutspath move patharray =
  match move with
  |Move(y, x) -> false
  |PlaceWall wlist -> let rec checkwalls = function
                      |[] -> false
                      |(wy, wx)::wtl -> patharray.(wy).(wx) || checkwalls wtl
                    in checkwalls wlist

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
  ) in

  (* Filter out invalid moves *)
  let parray = pathtoarray (path_to_win board player_id) board.size in
  let oarray = pathtoarray (path_to_win board (1 - player_id)) board.size in

  List.filter
  (fun m -> match m with
            |Move(y, x) -> validate_move player_id m board
            |PlaceWall wlist -> begin
              if (nwalls = 0) then false else
              let rec canplace = function
                |[] -> true
                |(y, x)::tl -> board.board.(y).(x) <> Wall && canplace tl in
              if (not (canplace wlist)) then false else
              if (cutspath m parray) || (cutspath m oarray) then
              validate_move player_id m board else true
            end)
  (moves @ wall_placements)


let rec printpath = function
  | [] -> ()
  | [(y, x)] -> Printf.printf "(%d, %d)\n%!" x y
  | (y, x)::tl -> (Printf.printf "(%d, %d)--" x y); printpath tl



let printparray patharray =
  Array.iter(fun y ->
    Array.iter(fun x -> Printf.printf "%d " (if x then 1 else 0)
    ) y; print_newline()
  ) patharray

(* Returns a list of movements that achieve the maximal value of minimal value
the opponent can force the player into, where the value of a position is defined
as ???????? *)

let ismove = function
  |Move(x, y) -> true
  |_ -> false

let heuristic game player_id =
  let odist = dist_to_win game (1 - player_id) in
  let pdist = dist_to_win game player_id in
  Printf.printf "%d %d\n%!" pdist odist;
  -((100 - odist) * (100 - odist)) + ((100 - pdist) * (100 - pdist))

(* https://en.wikipedia.org/wiki/Alpha–beta_pruning *)
let minimax game player_id =
  let prev_loc = fst game.players.(player_id) in
  let rec alphabeta game depth alpha beta maximizing = (* returns (score, move) *)
    if depth = 0 then
      (heuristic game player_id, None)
    else if maximizing = true then
      let moves = get_valid_moves game player_id in
      let find_best (best, best_moves, alpha) move =
        if beta <= alpha then
          (best, best_moves, alpha)
        else
          let () = commit_move player_id move game in
          let (score,_) = alphabeta game (depth - 1) alpha beta (not maximizing) in
          let () = undo player_id move game prev_loc in
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
      let moves = get_valid_moves game player_id in
      let find_best (best, best_moves, beta) move =
        if beta <= alpha then
          (best, best_moves, beta)
        else
          let () = commit_move player_id move game in
          let (score,_) = alphabeta game (depth - 1) alpha beta (not maximizing) in
          let () = undo player_id move game prev_loc in
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
  match alphabeta game 2 min_int max_int true with
  | (_,Some best_moves) -> best_moves
  | _ -> failwith "impossible"

(*
let minimax game player_id =
  let op_id = 1 - player_id in
  let pml = get_valid_moves game player_id in
  if List.length pml = 0 then Printf.printf "Oopsy daisy!\n%!";
  let (odist, pdist) = (dist_to_win game op_id, dist_to_win game player_id) in
  (Printf.printf "now pdist = %d, odist = %d\n%!" pdist odist);
  let ppath = pathtoarray (path_to_win game player_id) game.size in
  let opath = pathtoarray (path_to_win game op_id) game.size in
  let (ploc, oloc) = (fst game.players.(player_id), fst game.players.(op_id)) in
  let counter = ref 0 in
  let rec best game = function
    |[] -> (-max_int, [])
    |pm::ptl -> (commit_move player_id pm game);
          (* let t = Sys.time() in *)
          let oml = get_valid_moves game op_id in
          (* (Printf.printf "get_valid_moves took %fs seconds\n%!" (Sys.time() -. t)); *)
          let rec worse b = function
          |[] -> max_int
          |om::otl -> (commit_move op_id om b);
            let x = if (cutspath om ppath) || (cutspath pm ppath) || ismove pm
                    then ((incr counter); dist_to_win b player_id) else pdist in
            let y = if (cutspath om opath) || (cutspath pm opath) || ismove om
                    then ((incr counter); dist_to_win b op_id) else odist in
            let a = if (y <= 8) then y*(16-y) else 4*y + 32 in
            let d = a - 8*x in (undo op_id om b oloc); min d (worse b otl) in
          (* let t = Sys.time() in *)
          let w = worse game oml in
          (* (Printf.printf "worse game took %fs seconds\n%!" (Sys.time() -. t)); *)
          (undo player_id pm game ploc);
          let (prevbest, bestmoves) = best game ptl in
          if (w = prevbest) then (w, pm::bestmoves) else
          if (w > prevbest) then (w, [pm]) else (prevbest, bestmoves)
  in let ans = snd (best game pml) in
  (Printf.printf "Called dist_to_win %d times!\n%!" !counter); ans
*)

let next_move game player_id =
  let t = Sys.time() in let ans =
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
  in (Printf.printf "Thinking time: %fs\n%!" (Sys.time() -. t)); ans
