(* let in_file = open_in "data/day1" in *)
(* let contents = In_channel.input_lines in_file in *)
(* close_in in_file; *)
(* module Day1 : sig *)
(*     val get_input : string list *)
(* end *)

let data =
  let in_file = open_in "data/day1" in
  let contents = In_channel.input_lines in_file in
  close_in in_file;
  contents

let separate_id (line : string) =
  let ws x = x == ' ' in
  let str_list = List.init (String.length line) (String.get line) in
  let first = List.take_while (fun x -> not (ws x)) str_list in
  let str_list =
    List.drop_while ws (List.drop_while (fun x -> not (ws x)) str_list)
  in
  let second = List.take_while (fun x -> not (ws x)) str_list in

  ( int_of_string (String.of_seq (List.to_seq first)),
    int_of_string (String.of_seq (List.to_seq second)) )

let solve_part1 (data : string list) =
  let pairs = List.map separate_id data in
  print_endline "split succcess";
  let left, right = List.split pairs in
  let f acc a b = acc + abs (a - b) in
  List.fold_left2 f 0 (List.sort compare left) (List.sort compare right)

let solve_part2 (data : string list) =
  let left, right = List.split (List.map separate_id data) in
  let f a = a * List.length (List.filter (fun b -> b == a) right) in
  let freqs = List.map f left in
  print_endline (string_of_int (List.length freqs));

  List.fold_left (fun acc x -> acc + x) 0 freqs
