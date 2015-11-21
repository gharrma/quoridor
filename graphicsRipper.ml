open Graphics

let rec hwrip chan a =
  match (input_char chan)with
    |'\n' -> int_of_string(a)
    | v   -> hwrip chan (a^Char.escaped v)

let rip (name:string)=
  let file = name^".pbm" in
  let ic = open_in file in
  for i = 0 to 2 do (* Rip PBM header *)
    ignore(input_char ic);
  done;
  let w = hwrip ic "" in
  let h = hwrip ic "" in
  for i = 0 to 3 do (* Rip color header *)
    ignore(input_char ic);
  done;
  let z = Array.make_matrix h w white in
  for i = 0 to (h - 1) do
    for j = 0 to (w - 1) do
      let r = int_of_char(input_char ic) in
      let g = int_of_char(input_char ic) in
      let b = int_of_char(input_char ic) in
      let rgbColor = rgb r g b in
      z.(i).(j) <- if rgbColor = 16711935 then (-1) else rgbColor;
    done;
  done;
  close_in ic;
  z