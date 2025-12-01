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

type direction = North | South | East | West

let direction_to_offset = function
  | North -> (-1, 0)
  | South -> (1, 0)
  | East -> (0, 1)
  | West -> (0, -1)

let change_direction = function
  | North -> East
  | East -> South
  | South -> West
  | West -> North

let string_of_direction = function
  | North -> "North"
  | South -> "South"
  | East -> "East"
  | West -> "West"

module IntPairs = struct
  type t = int * int

  let compare (x0, y0) (x1, y1) =
    match Stdlib.compare x0 x1 with 0 -> Stdlib.compare y0 y1 | c -> c
end

module CoordDirection = struct
  type t = (int * int) * direction

  let compare ((x0, y0), d0) ((x1, y1), d1) =
    if d0 == d1 then
      match Stdlib.compare x0 x1 with 0 -> Stdlib.compare y0 y1 | c -> c
    else -1
end

module PairSet = Set.Make (IntPairs)
module PositionSet = Set.Make (CoordDirection)

let bounds_check (grid : char array array) y x : bool =
  Array.length grid > 0
  && y < Array.length grid
  && y >= 0
  && x < Array.length grid.(0)
  && x >= 0

let rec trace_guard_route (grid : char array array) (pos : int * int)
    (d : direction) (visited : PairSet.t) : int =
  let offset = direction_to_offset d in
  let next_pos = (fst pos + fst offset, snd pos + snd offset) in

  if not (bounds_check grid (fst next_pos) (snd next_pos)) then
    List.length PairSet.(add pos visited |> elements)
  else
    let hit_wall () = trace_guard_route grid pos (change_direction d) visited in

    match grid.(fst next_pos).(snd next_pos) with
    | '#' -> hit_wall ()
    | _ -> trace_guard_route grid next_pos d (PairSet.add pos visited)

let rec trace_guard_route_list (grid : char array array) (pos : int * int)
    (d : direction) (visited : PairSet.t) : PairSet.t =
  let offset = direction_to_offset d in
  let next_pos = (fst pos + fst offset, snd pos + snd offset) in

  if not (bounds_check grid (fst next_pos) (snd next_pos)) then
    PairSet.add pos visited
  else
    let hit_wall () =
      trace_guard_route_list grid pos (change_direction d) visited
    in

    match grid.(fst next_pos).(snd next_pos) with
    | '#' -> hit_wall ()
    | _ -> trace_guard_route_list grid next_pos d (PairSet.add pos visited)

let find_guard_start (grid : char array array) : (int * int) option =
  let x = ref 0 in

  let y =
    Array.find_index
      (fun a ->
        match Array.find_index (fun c -> c == '^') a with
        | Some i ->
            x := i;
            true
        | None -> false)
      grid
  in
  match y with Some i -> Some (i, !x) | None -> None

let solve_part1 () =
  let data = get_data "data/day6" in
  let grid =
    Array.of_list (List.map (fun s -> Array.of_list (explode s)) data)
  in

  let tpos = (47, 42) in
  let offset = direction_to_offset North in
  let npos = (fst tpos + fst offset, snd tpos + snd offset) in
  Printf.printf "npos: %d %d\n" (fst npos) (snd npos);

  match find_guard_start grid with
  | Some (y, x) ->
      Printf.printf "%d %d\n" y x;
      trace_guard_route grid (y, x) North (PairSet.of_list [ (y, x) ])
  | None -> 0

let rec trace_for_loops grid start obs =
  let rec _loop_trace p d turns =
    let offset = direction_to_offset d in
    let next_pos = (fst p + fst offset, snd p + snd offset) in
    if not (bounds_check grid (fst next_pos) (snd next_pos)) then false
    else if PositionSet.mem (p, d) turns then true
    else
      match grid.(fst next_pos).(snd next_pos) with
      | '#' -> _loop_trace p (change_direction d) (PositionSet.add (p, d) turns)
      | _ -> _loop_trace next_pos d turns
  in

  let c = grid.(fst obs).(snd obs) in
  grid.(fst obs).(snd obs) <- '#';
  let result = _loop_trace start North PositionSet.empty in
  grid.(fst obs).(snd obs) <- c;

  result

let solve_part2 () =
  let data = get_data "data/day6" in
  let grid =
    Array.of_list (List.map (fun s -> Array.of_list (explode s)) data)
  in

  match find_guard_start grid with
  | Some (y, x) ->
      let cells_visited =
        trace_guard_route_list grid (y, x) North (PairSet.of_list [ (y, x) ])
      in
      Printf.printf "start %d %d\n" y x;
      Printf.printf "visited len: %d\n"
        (List.length (PairSet.to_list cells_visited));

      List.fold_left
        (fun acc p -> if trace_for_loops grid (y, x) p then acc + 1 else acc)
        0
        (PairSet.to_list cells_visited)
  | None -> 0

let () = print_int (solve_part1 ())
let () = print_endline ""
let () = print_int (solve_part2 ())
let () = print_endline ""
