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

type word_state = X | M | A | S

let letter (s : word_state) =
  match s with X -> 'X' | M -> 'M' | A -> 'A' | S -> 'S'

let next_state (s : word_state) =
  match s with X -> M | M -> A | A -> S | S -> S

let bounds_check (grid : char array array) (x : int) (y : int) =
  let x_len, y_len = (Array.length grid, Array.length grid.(0)) in
  x >= 0 && x < x_len && y >= 0 && y < y_len

let rec check_cell (grid : char array array) x y (dir : int * int)
    (state : word_state) : word_state * bool =
  let dx, dy = (fst dir + x, snd dir + y) in

  let eval_cell () =
    if letter state == grid.(x).(y) then
      match state with
      | S -> (S, true)
      | _ -> check_cell grid dx dy dir (next_state state)
    else (X, false)
  in

  if not (bounds_check grid x y) then (X, false) else eval_cell ()

let word_search (grid : char array array) : int =
  let directions =
    [
      (-1, -1);
      (-1, 0);
      (-1, 1);
      (0, -1);
      (0, 0);
      (0, 1);
      (1, -1);
      (1, 0);
      (1, 1);
    ]
  in

  let x_len, y_len = (Array.length grid, Array.length grid.(0)) in
  let sum = ref 0 in

  for i = 0 to x_len do
    for j = 0 to y_len do
      sum :=
        !sum
        + List.fold_left
            (fun acc d ->
              let _, found = check_cell grid i j d X in
              if found then acc + 1 else acc)
            0 directions
    done
  done;
  !sum

let solve =
  let data =
    Array.of_list
      (List.map
         (fun line -> Array.of_list (explode line))
         (get_data "data/day4"))
  in

  word_search data

type diag = Top | Bottom

let get_indices diag x y =
  match diag with
  | Top -> ((x - 1, y - 1), (x + 1, y + 1))
  | Bottom -> ((x - 1, y + 1), (x + 1, y - 1))

let check_diagonal grid x y : bool =
  let eval lpair rpair =
    let l_char = grid.(fst lpair).(snd lpair) in
    let r_char = grid.(fst rpair).(snd rpair) in
    match l_char with
    | 'M' -> if r_char == 'S' then true else false
    | 'S' -> if r_char == 'M' then true else false
    | _ -> false
  in

  let topl, topr = get_indices Top x y in
  let botl, botr = get_indices Bottom x y in

  eval topl topr && eval botl botr

let x_search (grid : char array array) : int =
  let x_len, y_len = (Array.length grid, Array.length grid.(0)) in
  let sum = ref 0 in
  for i = 1 to x_len - 2 do
    for j = 1 to y_len - 2 do
      let c = grid.(i).(j) in
      if c == 'A' && check_diagonal grid i j then sum := !sum + 1
    done
  done;
  !sum

let solve_part_two =
  let data =
    Array.of_list
      (List.map
         (fun line -> Array.of_list (explode line))
         (get_data "data/day4"))
  in

  x_search data

let () = print_endline (string_of_int solve)
let () = print_endline (string_of_int solve_part_two)
