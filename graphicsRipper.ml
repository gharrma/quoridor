open Graphics

let rec hwrip chan a =
  match (input_char chan)with
    |'\n' -> int_of_string(a)
    | v   -> hwrip chan (a^Char.escaped v)

let overlay (c1:int) (c2:int) =
  if c1 < 128
  then int_of_float(float_of_int((c1*c2))/.127.5)
  else 255 - int_of_float(float_of_int((255-c1)*(255-c2))/.127.5)

let rip (name:string) =
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

let ripColorize (name:string) (red:int) (green:int) (blue:int) =
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
      let r2 = overlay r red in
      let g2 = overlay g green in
      let b2 = overlay b blue in
      let rgbColor2 = rgb r2 g2 b2 in
      z.(i).(j) <- if rgbColor = 16711935 then (-1) else rgbColor2;
    done;
  done;
  close_in ic;
  z