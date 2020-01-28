open Core

module Tile = struct
  type t = Bug | Empty

  let of_char = function
    | '.' -> Empty
    | '#' -> Bug
    | c -> Printf.sprintf "Unknown tile: %c" c |> failwith

  let of_int = function
    | 0 -> Empty
    | 1 -> Bug
    | n -> Printf.sprintf "Unknown tile: %i" n |> failwith

  let to_int = function Empty -> 0 | Bug -> 1

  let is_bug = function Bug -> true | Empty -> false

  let next (tile : t) (neighbours : int) : t =
    match tile with
    | Bug when neighbours <> 1 -> Empty
    | Empty when neighbours = 1 || neighbours = 2 -> Bug
    | _ -> tile
end

module Grid = struct
  type t = int

  let dimy = 5

  let dimx = 5

  let shift ~x ~y = (y * dimx) + x

  let set_exn (g : t) ~x ~y ~(v : Tile.t) =
    let v = Tile.to_int v in
    let shift = shift ~x ~y in
    let mask = lnot (1 lsl shift) in
    g land mask lor (v lsl shift)

  let at_exn (g : t) ~x ~y =
    let shift = shift ~x ~y in
    (g lsr shift) land 1 |> Tile.of_int

  let at (g : t) ~(default : Tile.t) ~x ~y =
    if x < 0 || x >= dimx || y < 0 || y >= dimy then default else at_exn g ~x ~y

  let hash (g : t) = g

  let init (input : string list) : t =
    let g = ref 0 in
    for i = 0 to (dimx * dimy) - 1 do
      let y = i / dimx in
      let x = i % dimx in
      let v = List.nth_exn input y |> Fn.flip String.get x |> Tile.of_char in
      g := set_exn !g ~x ~y ~v
    done;
    !g

  let neighbors ~x ~y = [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ]

  let iteration (g : t) =
    let next_g = ref g in
    for y = 0 to dimy - 1 do
      for x = 0 to dimx - 1 do
        let v =
          neighbors ~x ~y
          |> List.map ~f:(fun (x, y) -> at g ~default:Tile.Empty ~x ~y)
          |> List.count ~f:Tile.is_bug
          |> Tile.next (at_exn g ~x ~y)
        in
        next_g := set_exn !next_g ~x ~y ~v
      done
    done;
    !next_g
end

let find_first_repeated_layout g =
  let rec loop biodiversities g =
    let h = Grid.hash g in
    if Set.exists biodiversities ~f:(Int.equal h) then h
    else
      let g = Grid.iteration g in
      loop (Set.add biodiversities h) g
  in
  let biodiversities = Set.empty (module Int) in
  loop biodiversities g

module RecursiveGrid = struct
  type t = (int, Grid.t, Int.comparator_witness) Map.t

  let init (g : Grid.t) : t = Map.of_alist_exn (module Int) [ (0, g) ]

  let bugs_count (rg : t) : int =
    Map.fold rg ~init:0 ~f:(fun ~key:_ ~data count -> count + Int.popcount data)

  let at_exn rg ~depth ~x ~y : Tile.t =
    Map.find_exn rg depth |> Grid.at_exn ~x ~y

  let at (rg : t) ~default ~depth ~x ~y =
    match Map.find rg depth with
    | None -> default
    | Some g -> Grid.at g ~default ~x ~y

  let neighbors ~depth ~x ~y =
    match Grid.shift ~x ~y with
    | 0 ->
        [
          (depth + 1, 2, 1);
          (depth, x, y + 1);
          (depth + 1, 1, 2);
          (depth, x + 1, y);
        ]
    | 4 ->
        [
          (depth + 1, 2, 1);
          (depth, x, y + 1);
          (depth, x - 1, y);
          (depth + 1, 3, 2);
        ]
    | 20 ->
        [
          (depth, x, y - 1);
          (depth + 1, 2, 3);
          (depth + 1, 1, 2);
          (depth, x + 1, y);
        ]
    | 24 ->
        [
          (depth, x, y - 1);
          (depth + 1, 2, 3);
          (depth, x - 1, y);
          (depth + 1, 3, 2);
        ]
    | 1 | 2 | 3 ->
        [
          (depth + 1, 2, 1);
          (depth, x, y + 1);
          (depth, x - 1, y);
          (depth, x + 1, y);
        ]
    | 21 | 22 | 23 ->
        [
          (depth, x, y - 1);
          (depth + 1, 2, 3);
          (depth, x - 1, y);
          (depth, x + 1, y);
        ]
    | 5 | 10 | 15 ->
        [
          (depth, x, y - 1);
          (depth, x, y + 1);
          (depth + 1, 1, 2);
          (depth, x + 1, y);
        ]
    | 9 | 14 | 19 ->
        [
          (depth, x, y - 1);
          (depth, x, y + 1);
          (depth, x - 1, y);
          (depth + 1, 3, 2);
        ]
    | 7 ->
        [
          (depth, x, y - 1);
          (depth - 1, 0, 0);
          (depth - 1, 1, 0);
          (depth - 1, 2, 0);
          (depth - 1, 3, 0);
          (depth - 1, 4, 0);
          (depth, x - 1, y);
          (depth, x + 1, y);
        ]
    | 11 ->
        [
          (depth, x, y - 1);
          (depth, x, y + 1);
          (depth, x - 1, y);
          (depth - 1, 0, 0);
          (depth - 1, 0, 1);
          (depth - 1, 0, 2);
          (depth - 1, 0, 3);
          (depth - 1, 0, 4);
        ]
    | 13 ->
        [
          (depth, x, y - 1);
          (depth, x, y + 1);
          (depth - 1, 4, 0);
          (depth - 1, 4, 1);
          (depth - 1, 4, 2);
          (depth - 1, 4, 3);
          (depth - 1, 4, 4);
          (depth, x + 1, y);
        ]
    | 17 ->
        [
          (depth - 1, 0, 4);
          (depth - 1, 1, 4);
          (depth - 1, 2, 4);
          (depth - 1, 3, 4);
          (depth - 1, 4, 4);
          (depth, x, y + 1);
          (depth, x - 1, y);
          (depth, x + 1, y);
        ]
    | 6 | 8 | 16 | 18 ->
        [
          (depth, x, y - 1);
          (depth, x, y + 1);
          (depth, x - 1, y);
          (depth, x + 1, y);
        ]
    | _ -> failwith "Neighbors in a below level"

  let pad (rg : t) : t =
    let depth, g = Map.min_elt_exn rg in
    let rg = if g = 0 then rg else Map.add_exn rg ~key:(depth - 1) ~data:0 in
    let depth, g = Map.max_elt_exn rg in
    let rg = if g = 0 then rg else Map.add_exn rg ~key:(depth + 1) ~data:0 in
    rg

  let iteration (rg : t) : t =
    let iter_one ~depth (rg : t) : Grid.t =
      List.init (Grid.dimx * Grid.dimy) ~f:Fn.id
      |> List.fold ~init:0 ~f:(fun new_g ->
           function
           | 12 -> new_g
           | shift ->
               let y = shift / Grid.dimx in
               let x = shift % Grid.dimx in
               neighbors ~depth ~x ~y
               |> List.map ~f:(fun (depth, x, y) ->
                      at rg ~default:Tile.Empty ~depth ~x ~y)
               |> List.count ~f:Tile.is_bug
               |> Tile.next (at_exn rg ~depth ~x ~y)
               |> fun tile -> Grid.set_exn new_g ~x ~y ~v:tile)
    in
    let padded_rg = pad rg in
    Map.fold padded_rg
      ~init:(Map.empty (module Int))
      ~f:(fun ~key ~data:_ next ->
        let data = iter_one ~depth:key padded_rg in
        Map.add_exn next ~key ~data)
end

let () =
  let g =
    In_channel.read_lines "./input.txt" |> List.map ~f:String.strip |> Grid.init
  in
  find_first_repeated_layout g |> Printf.printf "part 1: %i\n";
  RecursiveGrid.init g
  |> Fn.apply_n_times ~n:200 RecursiveGrid.iteration
  |> RecursiveGrid.bugs_count |> Printf.printf "part 2: %i"
