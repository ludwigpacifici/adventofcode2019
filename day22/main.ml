open Core

module Technique = struct
  type t = Cut of int | Deal_into_new_stack | Deal_with_increment of int

  let of_string (s : string) : t =
    match String.split ~on:' ' s with
    | [ "deal"; "into"; "new"; "stack" ] -> Deal_into_new_stack
    | [ "cut"; n ] -> Cut (Int.of_string n)
    | [ "deal"; "with"; "increment"; n ] ->
        Deal_with_increment (Int.of_string n)
    | _ -> failwith ("Unknown technique: " ^ s)
end

let part1 moves =
  let deal_into_new_stack cards = Array.rev_inplace cards in
  let cut cards n =
    let len = Array.length cards in
    let n = ((n % len) + len) % len in
    let cards2 = Array.copy cards in
    Array.blit ~src:cards2 ~src_pos:0 ~dst:cards ~dst_pos:(len - n) ~len:n;
    Array.blit ~src:cards2 ~src_pos:n ~dst:cards ~dst_pos:0 ~len:(len - n)
  in
  let deal_with_increment cards n =
    let cards2 = Array.copy cards in
    Array.iteri cards2 ~f:(fun i x -> cards.(i * n % Array.length cards) <- x)
  in
  let apply cards = function
    | Technique.Deal_into_new_stack -> deal_into_new_stack cards
    | Cut n -> cut cards n
    | Deal_with_increment n -> deal_with_increment cards n
  in
  let shuffle cards ts = List.iter ts ~f:(apply cards) in
  let cards = Array.init 10_007 ~f:Fn.id in
  shuffle cards moves;
  let i, _ = Array.findi_exn cards ~f:(fun _ x -> x = 2019) in
  i

(* Modular exponentiation: x^y mod m
 * https://www.geeksforgeeks.org/modular-exponentiation-power-in-modular-arithmetic/ *)
let powm x y m =
  let open Int64 in
  let result = ref 1L in
  let x = ref (x % m) in
  let y = ref y in
  while !y > 0L do
    if !y % 2L = 1L then result := !result * !x % m;
    y := !y / 2L;
    x := !x * !x % m
  done;
  !result

(* https://www.reddit.com/r/adventofcode/comments/ee0rqi/2019_day_22_solutions/fbnkaju/ *)
let part2 moves =
  let open Int64 in
  let cards = 119_315_717_514_047L in
  let get i (offset, increment) = (offset + (i * increment)) % cards in
  let inv n = powm n (cards - 2L) cards in
  let deal_into_new_stack (offset, increment) =
    let inc = -1L * increment % cards in
    ((offset + inc) % cards, inc)
  in
  let cut n (offset, increment) =
    ((offset + (increment * n)) % cards, increment)
  in
  let deal_with_increment n (offset, increment) =
    (offset, increment * inv n % cards)
  in
  let apply (offset, increment) = function
    | Technique.Deal_into_new_stack -> deal_into_new_stack (offset, increment)
    | Cut n -> cut (Int64.of_int n) (offset, increment)
    | Deal_with_increment n ->
        deal_with_increment (Int64.of_int n) (offset, increment)
  in
  let repeat n (offset, increment) =
    let inc = powm increment n cards in
    let offset = offset * (1L - inc) * inv ((1L - increment) % cards) in
    (offset % cards, inc)
  in
  let shuffle ts = List.fold ~init:(0L, 1L) ts ~f:apply in
  shuffle moves |> repeat 101_741_582_076_661L |> get 2020L

let () =
  let moves =
    In_channel.read_lines "./input.txt"
    |> List.map ~f:String.strip
    |> List.map ~f:Technique.of_string
  in
  part1 moves |> Printf.printf "part 1: %i\n";
  part2 moves |> Printf.printf "part 2: %Li\n"
