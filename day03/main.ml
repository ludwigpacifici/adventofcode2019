open Core

module Coordinate = struct
  module T = struct
    type t = int * int [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)

  let direction = function
    | 'u' -> fun (x, y) -> (x, y + 1)
    | 'd' -> fun (x, y) -> (x, y - 1)
    | 'l' -> fun (x, y) -> (x - 1, y)
    | 'r' -> fun (x, y) -> (x + 1, y)
    | d -> failwith ("Unknown direction: " ^ String.of_char d)
end

let read path = (path.[0], String.drop_prefix path 1 |> Int.of_string)

let run_step panel start stepi (direction, steps) =
  let d = Coordinate.direction direction in
  let rec loop panel prev stepi = function
    | n when n > 0 ->
        let c = d prev in
        let panel =
          Map.update panel c ~f:(Option.value ~default:(false, stepi))
        in
        loop panel c (stepi + 1) (n - 1)
    | _ -> (panel, prev, stepi)
  in
  loop panel start stepi steps

let central_port = (0, 0)

let merge p1 p2 =
  let panel = Map.empty (module Coordinate) in
  Map.fold2 p1 p2 ~init:panel ~f:(fun ~key ~data panel ->
      match data with
      | `Both ((_, s1), (_, s2)) -> Map.add_exn panel ~key ~data:(true, s1 + s2)
      | `Left (_, s) | `Right (_, s) -> Map.add_exn panel ~key ~data:(false, s))

let unroll_wire wire =
  let panel = Map.empty (module Coordinate) in
  List.map wire ~f:read
  |> List.fold ~init:(panel, central_port, 1)
       ~f:(fun (panel, position, stepi) c -> run_step panel position stepi c)
  |> fun (panel, _, _) -> panel

let manhattan_distance (x, y) = abs x + abs y

let part1 ~key ~data:_ min_distance =
  let distance = manhattan_distance key in
  if distance > 0 then min distance min_distance else min_distance

let part2 ~key:_ ~data min_distance =
  if data > 0 then min min_distance data else min_distance

let twist_and_turn ~find input =
  let panel = Map.empty (module Coordinate) in
  List.map input ~f:(String.split ~on:',')
  |> List.fold ~init:panel ~f:(fun panel wire ->
         unroll_wire wire |> merge panel)
  |> Map.filter ~f:fst |> Map.map ~f:snd
  |> Map.fold ~init:Int.max_value ~f:find

let () =
  let input =
    In_channel.read_lines "./input.txt"
    |> Fn.flip List.take 2 |> List.map ~f:String.strip
    |> List.map ~f:String.lowercase
  in
  twist_and_turn ~find:part1 input |> Printf.printf "part 1: %i\n";
  twist_and_turn ~find:part2 input |> Printf.printf "part 2: %i\n"
