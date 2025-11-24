(* The levels are either all increasing or all decreasing. *)
(* Any two adjacent levels differ by at least one and at most three. *)

let get_data path =
  let in_file = open_in path in
  let contents = In_channel.input_lines in_file in
  close_in in_file;
  contents

type dir = Inc | Dec

let string_of_dir (d : dir) : string =
  match d with Inc -> "Increment" | Dec -> "Decrement"

let check_range n = n >= 1 && n <= 3

let check_direction vs =
  match vs with
  | x :: y :: v -> if x < y then Inc else Dec
  | _ -> failwith "Expected atleast 2 numbers"

let in_range x y =
  let dist = abs (x - y) in
  dist >= 1 && dist <= 3

let is_valid x y dir =
  let valid_range = in_range x y in
  valid_range && ((x > y && dir == Dec) || (x < y && dir == Inc))

let rec eval_line nums direction =
  match nums with
  | x :: y :: t ->
      if is_valid x y direction then eval_line (y :: t) direction else false
  | _ -> true

let process_line line =
  List.map (fun x -> int_of_string x) (String.split_on_char ' ' line)

let solve_part1 =
  let data = get_data "data/day2" in
  List.fold_left
    (fun acc x ->
      let nums = process_line x in
      let dir = check_direction nums in
      if eval_line nums dir then acc + 1 else acc)
    0 data

let rec count_errors nums acc prev_dir =
  match nums with
  | x :: y :: t ->
      let dir = check_direction nums in
      let errs =
        if dir != prev_dir then 1 else 0 + if in_range x y then 0 else 1
      in
      count_errors (y :: t) (acc + errs) dir
  | _ -> acc

let rec _salvage (l : 'a list) (x : 'a) (r : 'a list) : bool =
  match (l, x, r) with
  | h, v, [] -> if eval_line h (check_direction h) then true else false
  | [], v, t ->
      if eval_line t (check_direction t) then true
      else _salvage (l @ [ x ]) (List.hd t) (List.tl t)
  | h, v, t ->
      if eval_line (h @ t) (check_direction (h @ t)) then true
      else _salvage (h @ [ v ]) (List.hd t) (List.tl t)

let salvage_line nums = _salvage [] (List.hd nums) (List.tl nums)

let solve_part2 =
  let data = get_data "data/day2" in

  List.fold_left
    (fun acc x ->
      let nums = process_line x in
      if count_errors nums 0 (check_direction nums) == 0 || salvage_line nums
      then acc + 1
      else acc)
    0 data

let () = print_endline (string_of_int solve_part1)
let () = print_endline (string_of_int solve_part2)
