open Core

let rec contains_two_adjacent = function
  | [] -> false
  | x :: y :: _ when x = y -> true
  | _ :: tl -> contains_two_adjacent tl

let rec contains_strict_two_multiple = function
  | [] -> false
  | [ x; y ] when x = y -> true
  | x :: y :: z :: _ when x = y && x <> z -> true
  | x :: y :: z :: _ as tl when x = y && x = z ->
      List.drop_while tl ~f:(Char.equal x) |> contains_strict_two_multiple
  | _ :: tl -> contains_strict_two_multiple tl

let rec is_increasing = function
  | [] -> true
  | x :: y :: _ when x > y -> false
  | _ :: tl -> is_increasing tl

let part1 p =
  let cs = Int.to_string p |> String.to_list in
  contains_two_adjacent cs && is_increasing cs

let part2 p =
  let cs = Int.to_string p |> String.to_list in
  contains_strict_two_multiple cs && is_increasing cs

let count ~f start stop =
  List.init (stop - start + 1) ~f:(fun x -> x + start) |> List.count ~f

let () =
  let start = 172930 in
  let stop = 683082 in
  count ~f:part1 start stop |> Printf.printf "part 1: %i\n";
  count ~f:part2 start stop |> Printf.printf "part 2: %i\n"
