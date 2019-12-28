open Core

module Coordinates = struct
  module T = struct
    type t = int * int [@@deriving sexp, compare, hash]
  end

  include T
  include Comparable.Make (T)
end

module Direction = struct
  type t = Up | Down | Left | Right [@@deriving enumerate]

  let move (x, y) = function
    | Up -> (x, y - 1)
    | Down -> (x, y + 1)
    | Left -> (x - 1, y)
    | Right -> (x + 1, y)
end

let is_door c = Char.is_alpha c && Char.is_uppercase c

let key_of_door door = Char.lowercase door

let is_key c = Char.is_alpha c && Char.is_lowercase c

module Maze = struct
  type t = { h : (Coordinates.t, char) Hashtbl.t; x_max : int; y_max : int }

  let empty () : t =
    { h = Hashtbl.create (module Coordinates); x_max = 0; y_max = 0 }

  let init (input : string) : t =
    String.fold input
      ~init:(empty (), (0, 0))
      ~f:(fun (m, (x, y)) -> function '\n' -> (m, (0, y + 1))
        | '#' ->
            ( { h = m.h; x_max = Int.max x m.x_max; y_max = Int.max y m.y_max },
              (x + 1, y) )
        | data ->
            Hashtbl.add_exn m.h ~key:(x, y) ~data;
            ( { h = m.h; x_max = Int.max x m.x_max; y_max = Int.max y m.y_max },
              (x + 1, y) ))
    |> fst

  let keys (m : t) : (char, Coordinates.t) Hashtbl.t =
    Hashtbl.fold m.h
      ~init:(Hashtbl.create (module Char))
      ~f:(fun ~key ~data acc ->
        if is_key data then Hashtbl.add_exn acc ~key:data ~data:key;
        acc)

  let find_starts (m : t) =
    Hashtbl.fold m.h ~init:[] ~f:(fun ~key ~data acc ->
        match data with '@' -> key :: acc | _ -> acc)
end

let bfs ~init ~acc ~f ~next ~visited =
  let q = Queue.create () in
  let rec loop q visited acc =
    match Queue.dequeue q with
    | None -> acc
    | Some x ->
        let q, visited = next q visited x in
        loop q visited (f acc x)
  in
  Queue.enqueue q init;
  loop q visited acc

module Cost = struct
  type t = { c : Coordinates.t; steps : int; collect : char list }

  exception Cost_found of t

  let value cost_cache (m : Maze.t) ~from ~target =
    let next maze q visited { c; steps; collect } =
      Direction.all
      |> List.map ~f:(Direction.move c)
      |> List.filter ~f:(fun c -> Set.mem visited c |> not)
      |> List.fold ~init:(q, visited) ~f:(fun (q, visited) c ->
             match Hashtbl.find maze c with
             | None -> (q, visited)
             | Some data when is_key data || is_door data ->
                 Queue.enqueue q
                   { c; steps = steps + 1; collect = data :: collect };
                 (q, Set.add visited c)
             | Some _ ->
                 Queue.enqueue q { c; steps = steps + 1; collect };
                 (q, Set.add visited c))
    in
    let f _ _ cost =
      if Coordinates.equal cost.c target then raise (Cost_found cost) else ()
    in
    try
      match Hashtbl.find cost_cache (from, target) with
      | Some x -> Some x
      | None ->
          let collect =
            Hashtbl.find_exn m.h from |> fun data ->
            if is_door data || is_key data then [ data ] else []
          in
          bfs
            ~init:{ c = from; steps = 0; collect }
            ~acc:() ~f:(f m.h) ~next:(next m.h)
            ~visited:(Set.empty (module Coordinates));
          None
    with Cost_found data ->
      Hashtbl.add_exn cost_cache ~key:(target, from) ~data;
      let data = { data with collect = List.rev data.collect } in
      Hashtbl.add_exn cost_cache ~key:(from, target) ~data;
      Some data
end

module Key_set = struct
  type t = int [@@deriving sexp, compare, hash]

  let to_bit_mask (ks : char) : int = Char.to_int ks - Char.to_int 'a'

  let to_char (i : int) : char = i + Char.to_int 'a' |> Char.of_int_exn

  let empty : t = 0

  let length (ks : t) : int = Int.popcount ks

  let mem (ks : t) (k : char) : bool =
    let mask = 1 lsl to_bit_mask k in
    mask land ks = mask

  let add (ks : t) (k : char) : t =
    let mask = 1 lsl to_bit_mask k in
    mask lor ks

  let diff (ks0 : t) (ks1 : t) : t = ks0 - ks1

  let is_subset (ks0 : t) (ks1 : t) = ks1 land ks0 = ks1

  let equal (ks0 : t) (ks1 : t) : bool = Int.equal ks0 ks1

  let to_list (ks : t) : char list =
    List.init 26 ~f:to_char |> List.filter ~f:(fun c -> mem ks c)
end

module Explore = struct
  type compute = Deffered | Done

  module Coordinates2 = struct
    module T = struct
      type t = Coordinates.t * Coordinates.t [@@deriving sexp, compare, hash]
    end

    include T
    include Comparable.Make (T)
  end

  module Coordinates_keys = struct
    module T = struct
      type t = Coordinates.t List.t * Key_set.t [@@deriving sexp, compare, hash]
    end

    include T
    include Comparable.Make (T)
  end

  type t = { cs : Coordinates.t array; steps : int; stash : Key_set.t }

  let start ~m ~from : int =
    let cost_cache = Hashtbl.create (module Coordinates2) in
    let key_location = Maze.keys m in
    let all_keys =
      List.fold (Hashtbl.keys key_location) ~init:Key_set.empty ~f:Key_set.add
    in
    let find_cost ~from ~target =
      Array.foldi from ~init:None ~f:(fun robot_i acc c ->
          match acc with
          | Some _ -> acc
          | None ->
              Cost.value cost_cache m ~from:c ~target
              |> Option.map ~f:(fun cost -> (target, robot_i, cost)))
    in
    let is_reachable (keys : Key_set.t) (collect : char list) =
      List.fold collect ~init:(Some keys) ~f:(fun keys data ->
          match (keys, data) with
          | None, _ -> None
          | Some keys, data when is_key data -> Some (Key_set.add keys data)
          | Some keys, data when is_door data ->
              if Key_set.mem keys (key_of_door data) then Some keys else None
          | _, _ -> None)
    in
    let remove_subet stashes =
      let rec loop acc = function
        | [] -> acc
        | (hd, _, _, _) :: tl
          when List.exists tl ~f:(fun (ks, _, _, _) -> Key_set.is_subset ks hd)
          ->
            loop acc tl
        | hd :: tl -> loop (hd :: acc) tl
      in
      loop [] stashes
    in
    let populate_queue q visited =
      List.iter (Hashtbl.keys visited) ~f:(fun cached_key ->
          match Hashtbl.find_exn visited cached_key with
          | _, Done -> ()
          | cached_data, Deffered ->
              Queue.enqueue q cached_data;
              Hashtbl.set visited ~key:cached_key ~data:(cached_data, Done))
    in
    let next _ q visited e =
      let visited =
        Key_set.diff all_keys e.stash
        |> Key_set.to_list
        |> List.map ~f:(Hashtbl.find_exn key_location)
        |> List.filter_map ~f:(fun target -> find_cost ~from:e.cs ~target)
        |> List.filter_map ~f:(fun (target, robot_i, cost) ->
               is_reachable e.stash cost.collect
               |> Option.map ~f:(fun stash -> (stash, target, robot_i, cost)))
        |> remove_subet
        |> List.fold ~init:visited
             ~f:(fun visited (stash, target, robot_i, cost) ->
               let new_c = Array.copy e.cs in
               new_c.(robot_i) <- target;
               Hashtbl.update visited
                 (Array.to_list new_c, stash)
                 ~f:(function
                   | Some ((cached_explore, _) as cached)
                     when cached_explore.steps < e.steps + cost.steps ->
                       cached
                   | _ ->
                       ( { cs = new_c; steps = e.steps + cost.steps; stash },
                         Deffered ));
               visited)
      in
      if Queue.is_empty q then populate_queue q visited;
      (q, visited)
    in
    let f _ min_steps e =
      if e.steps < min_steps && Key_set.equal all_keys e.stash then e.steps
      else min_steps
    in
    let init = { cs = from; steps = 0; stash = Key_set.empty } in
    bfs ~init ~acc:Int.max_value ~f:(f m) ~next:(next m)
      ~visited:(Hashtbl.create (module Coordinates_keys))
end

let shortest_path input_file =
  let input = In_channel.read_all input_file |> String.strip in
  let m = Maze.init input in
  let from = Maze.find_starts m |> List.to_array in
  Explore.start ~m ~from

let () =
  shortest_path "./input.txt" |> Printf.printf "part 1: %i\n";
  shortest_path "./input2.txt" |> Printf.printf "part 2: %i\n"
