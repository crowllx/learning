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

type rule = int * int list

let rec add_rule a b (r : rule list) : rule list =
  match r with
  | x :: xs ->
      if fst x == a then (fst x, b :: snd x) :: xs else x :: add_rule a b xs
  | [] -> [ (a, [ b ]) ]

let rec cmp_page a b (rs : rule list) =
  if List.exists (fun r -> if fst r == a then true else false) rs then
    let p = List.find (fun r -> if fst r == a then true else false) rs in

    if List.exists (fun n -> if n == b then true else false) (snd p) then true
    else false
  else false

let rec process_rules (data : string list) (rules : rule list) : rule list =
  let process_line line =
    let ns = String.split_on_char '|' line in
    let a = int_of_string (List.nth ns 0) in
    let b = int_of_string (List.nth ns 1) in
    add_rule a b rules
  in

  match data with x :: xs -> process_rules xs (process_line x) | [] -> rules

let valid_update (nums : int list) rules : bool =
  let rec validate = function
    | [] -> true
    | x :: xs ->
        if List.for_all (fun n -> not (cmp_page n x rules)) xs then validate xs
        else false
  in

  validate nums

let take_mid (nums : int list) : int =
  let mid_index = List.length nums / 2 in
  List.nth nums mid_index

let update_ints line : int list =
  List.map (fun n -> int_of_string n) (String.split_on_char ',' line)

let part_one () =
  let data = get_data "data/day5" in
  let rule_text = List.take_while (fun s -> String.length s > 0) data in
  let updates = List.drop (List.length rule_text + 1) data in

  let rules = process_rules rule_text [] in

  List.fold_left
    (fun acc s ->
      let ns = update_ints s in
      if valid_update ns rules then acc + take_mid ns else acc)
    0 updates

let part_two () =
  let data = get_data "data/day5" in
  let rule_text = List.take_while (fun s -> String.length s > 0) data in
  let updates = List.drop (List.length rule_text + 1) data in

  let rules = process_rules rule_text [] in

  List.fold_left
    (fun acc s ->
      let ns = update_ints s in
      let eql a b = if cmp_page a b rules then -1 else 1 in
      if not (valid_update ns rules) then acc + take_mid (List.sort eql ns)
      else acc)
    0 updates

let () = print_endline (string_of_int (part_one ()))
let () = print_endline (string_of_int (part_two ()))
