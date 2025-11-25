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

let eql c expect = c == expect

(* let get_num l = *)
(*   let a_str = *)
(*     List.take_while (fun c -> match c with '0' .. '9' -> true | _ -> false) l *)
(*   in *)
(*   let next_l = List.drop (List.length a_str + 1) l in *)
(*   let b_str = *)
(*     List.take_while *)
(*       (fun c -> match c with '0' .. '9' -> true | _ -> false) *)
(*       next_l *)
(*   in *)
(*   (a_str, b_str, List.drop (List.length b_str) next_l) *)

type keyword = Do | Don't | Number of int | None

let string_of_keyword k =
  match k with
  | Do -> "do"
  | Don't -> "don't"
  | Number n -> "number " ^ string_of_int n
  | None -> "none"

let rec check_num l n =
  let x = List.hd l in
  match x with
  | '0' .. '9' -> check_num (List.drop 1 l) (n @ [ x ])
  | _ -> (n, l)

let find_do l =
  let len = if List.length l < 7 then List.length l else 7 in
  let str = implode (List.take len l) in
  (* Printf.printf "%s\n" str; *)
  (* Printf.printf "%s \n" (implode (List.take 5 (List.drop 4 l))); *)
  if String.starts_with ~prefix:"do()" str then (Do, List.drop 3 l)
  else if String.starts_with ~prefix:"don't()" str then (Don't, List.drop 6 l)
  else
    let () = Printf.printf "%s\n" str in
    (None, l)

let rec check_next c e l a b =
  if List.length l > 0 then
    match (c, e) with
    | 'm', 'm' -> check_next (List.hd l) 'u' (List.tl l) 0 0
    | 'u', 'u' -> check_next (List.hd l) 'l' (List.tl l) 0 0
    | 'l', 'l' -> check_next (List.hd l) '(' (List.tl l) 0 0
    | '(', '(' ->
        let a_str, rem = check_num l [] in
        if List.length a_str > 0 then
          check_next (List.hd rem) ',' (List.tl rem)
            (int_of_string (implode a_str))
            0
        else check_next (List.hd rem) 'm' (List.tl rem) 0 0
    | ',', ',' ->
        let b_str, rem = check_num l [] in
        if List.length b_str > 0 && List.hd rem == ')' then
          (Number (a * int_of_string (implode b_str)), 0, rem)
        else check_next (List.hd rem) 'm' (List.tl rem) 0 0
    | 'd', _ -> (
        let res, rem = find_do (c :: l) in
        match res with
        | Do -> (Do, 0, rem)
        | Don't -> (Don't, 0, rem)
        | _ -> check_next (List.hd l) 'm' (List.tl l) 0 0)
    | _ -> check_next (List.hd l) 'm' (List.tl l) 0 0
  else (None, 0, l)

let check_length l =
  let len = List.length l in
  len >= 1 && len <= 3

let rec search (seq : char list) =
  let active = ref true in
  let rec _search (seq : char list) acc =
    match seq with
    | [] -> acc
    | xs -> (
        let word, n, rem = check_next (List.hd seq) 'm' seq 0 0 in
        match word with
        | Do ->
            active := true;
            _search rem acc
        | Don't ->
            active := false;
            _search rem acc
        | Number n ->
            let v = if !active then n else 0 in
            _search rem (acc + v)
        | None -> _search rem acc)
  in
  _search seq 0

let solve_part1 =
  let data = String.concat "" (get_data "data/day3") in
  search (explode data)

let () = print_endline (string_of_int solve_part1)
