let get_data path =
  let in_file = open_in path in
  let contents = In_channel.input_lines in_file in
  close_in in_file;
  contents

let explode s =
  let rec expl i l = if i < 0 then l else expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) []

let implode l =
  let buffer = Buffer.create (List.length l) in
  List.iter (Buffer.add_char buffer) l;
  Buffer.contents buffer

type rotation = L | R

let modulus a b =
  let rem = a mod b in
  if rem >= 0 then rem else rem + b

let rotate a b r =
  match r with L -> modulus (a - b) 100 | R -> modulus (a + b) 100

let parse_line s =
  let rot = s.[0] in
  let len = String.length s in
  let distance = int_of_string (String.sub s 1 (len - 1)) in

  match rot with
  | 'L' -> Some (L, distance)
  | 'R' -> Some (R, distance)
  | _ -> None

let solve_part1 () =
  let data = get_data "data/day1" in
  let dial = ref 50 in

  List.fold_left
    (fun acc line ->
      match parse_line line with
      | Some (r, d) ->
          dial := rotate !dial d r;
          if !dial == 0 then acc + 1 else acc
      | None -> acc)
    0 data

let count_clicks dial dist rot =
  match rot with
  | L -> (modulus (100 - dial) 100 + dist) / 100
  | R -> (dial + dist) / 100

let solve_part2 () =
  let data = get_data "data/day1" in
  let dial = ref 50 in

  List.fold_left
    (fun acc line ->
      match parse_line line with
      | Some (r, d) ->
          let clicks = count_clicks !dial d r in
          dial := rotate !dial d r;
          if clicks == 0 && !dial == 0 then
            Printf.printf "clicks %d %d\n" clicks !dial;
          acc + clicks
      | None -> acc)
    0 data

let () = print_endline (string_of_int (solve_part1 ()))
let () = print_endline (string_of_int (solve_part2 ()))
