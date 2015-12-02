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
    let (pos,_) = game.players.(player_id) in
    Move pos
    (* TODO: thanksgiving *)

(* Returns the distance of player with specified id fron the dest_locn according
 * to the current board configuration. Returns -1 if des_locn cannot be reached.
 *)
let distance_to_pos board player_id dest_locn =
    let (locn, nwalls) = (board.players).(player_id) in
    let q = Queue.create() in
    let vis = Hashtbl.create 100 in
    let dist = Hashtbl.create 100 in
    Queue.push locn q;
    let cur_nodes = ref 1 in
    let next_nodes = ref 0 in
    let depth = ref 0 in
    while (not (Queue.is_empty q)) && (not (Hashtbl.mem dist dest_locn)) do
        let (py, px) = Queue.pop q in
        Hashtbl.add vis (py, px) true;
        Hashtbl.add dist (py, px) !depth;
        cur_nodes := !cur_nodes - 1;
        if !cur_nodes = 0 then
            depth := !depth + 1;
            cur_nodes := !next_nodes;
            next_nodes := 0;
        let chk_neighbor neighbor =
            if not (Hashtbl.mem vis neighbor) then
                next_nodes := !next_nodes + 1;
                Queue.push neighbor q;
        in
        List.iter chk_neighbor [(py-2, px); (py+2, px); (py, px-2); (py, px+2)]
    done;
    try Hashtbl.find dist dest_locn with
    | Not_found -> -1
