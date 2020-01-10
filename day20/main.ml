open Core

module Coordinates2 = struct
  module T = struct
    type t = int * int [@@deriving sexp, compare, hash]
  end

  include T
  include Comparable.Make (T)
end

module Coordinates3 = struct
  module T = struct
    type t = int * int * int [@@deriving sexp, compare, hash]
  end

  include T
  include Comparable.Make (T)

  let of_coordinates2_z (x, y) z : t = (x, y, z)

  let to_coordinates2 (x, y, _) = (x, y)

  let z (_, _, v) = v
end

module Coordinates22 = struct
  module T = struct
    type t = Coordinates2.t * Coordinates2.t [@@deriving sexp, compare, hash]
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

module Maze = struct
  type outer = { x_min : int; y_min : int; x_max : int; y_max : int }

  type t = { h : (Coordinates2.t, char) Hashtbl.t; x_max : int; y_max : int }

  let empty () : t =
    { h = Hashtbl.create (module Coordinates2); x_max = 0; y_max = 0 }

  let init (input : string) : t =
    String.fold input
      ~init:(empty (), (0, 0))
      ~f:(fun (m, (x, y)) -> function '\n' -> (m, (0, y + 1))
        | '#' | ' ' ->
            ( { h = m.h; x_max = Int.max x m.x_max; y_max = Int.max y m.y_max },
              (x + 1, y) )
        | data ->
            Hashtbl.add_exn m.h ~key:(x, y) ~data;
            ( { h = m.h; x_max = Int.max x m.x_max; y_max = Int.max y m.y_max },
              (x + 1, y) ))
    |> fst

  let outer_min_max (m : t) : outer =
    Hashtbl.fold m.h
      ~init:
        { x_min = Int.max_value; y_min = Int.max_value; x_max = 0; y_max = 0 }
      ~f:(fun ~key ~data { x_min; y_min; x_max; y_max } ->
        let x, y = key in
        match data with
        | '.' ->
            {
              x_min = Int.min x_min x;
              y_min = Int.min y_min y;
              x_max = Int.max x_max x;
              y_max = Int.max y_max y;
            }
        | _ -> { x_min; y_min; x_max; y_max })
end

module Portals = struct
  type t = (string, Coordinates2.t list) Hashtbl.t

  let find_exn portals ~label = Hashtbl.find_exn portals label

  let find_one_exn portals ~label =
    Hashtbl.find_exn portals label
    |> List.hd_exn
    |> Fn.flip Coordinates3.of_coordinates2_z 0

  let start_exn portals = find_one_exn portals ~label:"AA"

  let finish_exn portals = find_one_exn portals ~label:"ZZ"

  let of_maze (m : Maze.t) =
    let portals = Hashtbl.create (module String) in
    let update p c0 c1 (x, y) =
      let key = String.of_char_list [ c0; c1 ] in
      let data = (x, y) in
      Hashtbl.update p key ~f:(fun o ->
          Option.value o ~default:[] |> fun l -> data :: l)
    in
    let vertical_portals x y =
      let p0 = Hashtbl.find m.h (x, y) in
      let p1 = Hashtbl.find m.h (x + 1, y) in
      let p2 = Hashtbl.find m.h (x + 2, y) in
      match (p0, p1, p2) with
      | Some c0, Some c1, Some '.' when Char.is_alpha c0 && Char.is_alpha c1 ->
          update portals c0 c1 (x + 2, y)
      | Some '.', Some c0, Some c1 when Char.is_alpha c0 && Char.is_alpha c1 ->
          update portals c0 c1 (x, y)
      | _ -> ()
    in
    let horizontal_portals x y =
      let p0 = Hashtbl.find m.h (x, y) in
      let p1 = Hashtbl.find m.h (x, y + 1) in
      let p2 = Hashtbl.find m.h (x, y + 2) in
      match (p0, p1, p2) with
      | Some c0, Some c1, Some '.' when Char.is_alpha c0 && Char.is_alpha c1 ->
          update portals c0 c1 (x, y + 2)
      | Some '.', Some c0, Some c1 when Char.is_alpha c0 && Char.is_alpha c1 ->
          update portals c0 c1 (x, y)
      | _ -> ()
    in
    for y = 0 to m.y_max do
      for x = 0 to m.x_max do
        vertical_portals x y;
        horizontal_portals x y
      done
    done;
    portals

  let portal_map (p : t) =
    Hashtbl.fold p
      ~init:(Hashtbl.create (module Coordinates2))
      ~f:(fun ~key:_ ~data acc ->
        match data with
        | [ x; y ] ->
            Hashtbl.set acc ~key:x ~data:y;
            Hashtbl.set acc ~key:y ~data:x;
            acc
        | _ -> acc)

  let portals_coordinates (p : t) =
    Hashtbl.fold p
      ~init:(Set.empty (module Coordinates2))
      ~f:(fun ~key:_ ~data acc ->
        Set.of_list (module Coordinates2) data |> Set.union acc)

  let is_outer ({ x_min; y_min; x_max; y_max } : Maze.outer) (x, y) =
    x = x_min || x = x_max || y = y_min || y = y_max

  let is_inner outer_min_max portal = not (is_outer outer_min_max portal)
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
  type t = { c : Coordinates2.t; steps : int }

  type cache = (Coordinates22.t, int option) Hashtbl.t

  let of_portals portal_map =
    let cache = Hashtbl.create (module Coordinates22) in
    Hashtbl.iteri portal_map ~f:(fun ~key ~data ->
        Hashtbl.add_exn cache ~key:(key, data) ~data:(Some 1));
    cache

  let value (c : cache) (m : Maze.t) ~from ~target =
    let next maze q visited { c; steps } =
      Direction.all
      |> List.map ~f:(Direction.move c)
      |> List.filter ~f:(fun c -> Set.mem visited c |> not)
      |> List.fold ~init:(q, visited) ~f:(fun (q, visited) c ->
             match Hashtbl.find maze c with
             | None -> (q, visited)
             | Some _ ->
                 Queue.enqueue q { c; steps = steps + 1 };
                 (q, Set.add visited c))
    in
    let f _ acc cost =
      if
        Coordinates2.equal cost.c target
        && Option.map acc ~f:(fun min_steps -> cost.steps < min_steps)
           |> Option.value ~default:true
      then Some cost.steps
      else acc
    in
    match Hashtbl.find c (from, target) with
    | Some x -> x
    | None ->
        if Coordinates2.equal from target then (
          Hashtbl.add_exn c ~key:(target, from) ~data:None;
          None )
        else
          let data =
            bfs ~init:{ c = from; steps = 0 } ~acc:None ~f:(f m.h)
              ~next:(next m.h)
              ~visited:(Set.empty (module Coordinates2))
          in
          Hashtbl.add_exn c ~key:(target, from) ~data;
          Hashtbl.add_exn c ~key:(from, target) ~data;
          data
end

let djikstra m c floor select_nodes ~from ~target =
  let distance = Hashtbl.create (module Coordinates3) in
  Hashtbl.set distance ~key:from ~data:0;
  let sort_q q =
    List.sort q ~compare:(fun x y -> Int.compare (fst x) (fst y))
  in
  let rec loop visited = function
    | [] -> ()
    | (_, a) :: _ when Coordinates3.equal a target -> ()
    | (_, a) :: q when Set.mem visited a -> loop visited q
    | (_, a) :: q ->
        let visited = Set.add visited a in
        let other_nodes = select_nodes a in
        Set.fold other_nodes ~init:q ~f:(fun q b2 ->
            let a2 = Coordinates3.to_coordinates2 a in
            match Cost.value c m ~from:a2 ~target:b2 with
            | None -> q
            | Some cost -> (
                let floor = floor a b2 in
                if floor < 0 then q
                else
                  let b = Coordinates3.of_coordinates2_z b2 floor in
                  let da = Hashtbl.find_exn distance a in
                  match Hashtbl.find distance b with
                  | None ->
                      Hashtbl.set distance ~key:b ~data:(da + cost);
                      (da + cost, b) :: q
                  | Some db when da + cost < db ->
                      Hashtbl.set distance ~key:b ~data:(da + cost);
                      (da + cost, b) :: q
                  | Some _ -> q ))
        |> sort_q |> loop visited
  in
  let visited = Set.empty (module Coordinates3) in
  let q = [ (0, from) ] in
  loop visited q;
  Hashtbl.find_exn distance target

let shortest_path m p _ cache start finish =
  let floor _ _ = 0 in
  let all_nodes = Portals.portals_coordinates p in
  let select_nodes _ = all_nodes in
  djikstra m cache floor select_nodes ~from:start ~target:finish

let rec_shortest_path m p portal_map cache start finish =
  let outer = Maze.outer_min_max m in
  let floor (x_a3, y_a3, z_a3) b2 =
    match Hashtbl.find portal_map (x_a3, y_a3) with
    | None -> 0
    | Some other_a2 ->
        let dz =
          if Coordinates2.equal other_a2 b2 then
            if Portals.is_outer outer (x_a3, y_a3) then -1 else 1
          else 0
        in
        z_a3 + dz
  in
  let all_nodes = Portals.portals_coordinates p in
  let nodes_lvl_not_0 =
    Set.remove all_nodes (Coordinates3.to_coordinates2 start)
    |> Fn.flip Set.remove (Coordinates3.to_coordinates2 finish)
  in
  let select_nodes a =
    if Coordinates3.z a = 0 then all_nodes else nodes_lvl_not_0
  in
  djikstra m cache floor select_nodes ~from:start ~target:finish

let () =
  let input = In_channel.read_all "./input.txt" in
  let m = Maze.init input in
  let p = Portals.of_maze m in
  let start = Portals.start_exn p in
  let finish = Portals.finish_exn p in
  let portal_map = Portals.portal_map p in
  let c = Cost.of_portals portal_map in
  shortest_path m p portal_map c start finish |> Printf.printf "part 1: %i\n";
  rec_shortest_path m p portal_map c start finish
  |> Printf.printf "part 2: %i\n"
