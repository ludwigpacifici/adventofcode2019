open Core

let bool_to_int (x : bool) : int = if x then 1 else 0

module IntCode = struct
  let ld ~mode ~relative_base code i =
    match mode with
    | 0 -> code.(code.(i))
    | 1 -> code.(i)
    | 2 -> code.(relative_base + code.(i))
    | n -> failwith ("Unknown ld mode: " ^ Int.to_string n)

  let st ~mode ~relative_base code i value =
    match mode with
    | 0 -> code.(code.(i)) <- value
    | 1 -> code.(i) <- value
    | 2 -> code.(relative_base + code.(i)) <- value
    | n -> failwith ("Unknown st mode: " ^ Int.to_string n)

  let read_instruction n =
    (n mod 100, n / 100 mod 10, n / 1_000 mod 10, n / 10_000 mod 10)

  type exit = { exit_code : int; outputs : int list }

  type suspended = { outputs : int list; ip : int; relative_base : int }

  type t = Exit of exit | Suspended of suspended

  let run ?(value0 = None) ?(noun = None) ?(verb = None) ?(inputs = [])
      ?(relative_base = 0) ?(ip = 0) (code : int Array.t) =
    let rec inner inputs outputs relative_base ip =
      match read_instruction code.(ip) with
      | 1, m1, m2, m3 ->
          ld ~mode:m1 ~relative_base code (ip + 1)
          + ld ~mode:m2 ~relative_base code (ip + 2)
          |> st ~mode:m3 ~relative_base code (ip + 3);
          inner inputs outputs relative_base (ip + 4)
      | 2, m1, m2, m3 ->
          ld ~mode:m1 ~relative_base code (ip + 1)
          * ld ~mode:m2 ~relative_base code (ip + 2)
          |> st ~mode:m3 ~relative_base code (ip + 3);
          inner inputs outputs relative_base (ip + 4)
      | 3, m, _, _ when m = 0 || m = 2 -> (
          match inputs with
          | [] -> Suspended { outputs = List.rev outputs; ip; relative_base }
          | i :: inputs ->
              st ~mode:m ~relative_base code (ip + 1) i;
              inner inputs outputs relative_base (ip + 2) )
      | 4, m, _, _ ->
          let out = ld ~mode:m ~relative_base code (ip + 1) in
          inner inputs (out :: outputs) relative_base (ip + 2)
      | 5, m1, m2, _ ->
          if ld ~mode:m1 ~relative_base code (ip + 1) <> 0 then
            ld ~mode:m2 ~relative_base code (ip + 2)
            |> inner inputs outputs relative_base
          else inner inputs outputs relative_base (ip + 3)
      | 6, m1, m2, _ ->
          if ld ~mode:m1 ~relative_base code (ip + 1) = 0 then
            ld ~mode:m2 ~relative_base code (ip + 2)
            |> inner inputs outputs relative_base
          else inner inputs outputs relative_base (ip + 3)
      | 7, m1, m2, m3 ->
          ld ~mode:m1 ~relative_base code (ip + 1)
          < ld ~mode:m2 ~relative_base code (ip + 2)
          |> bool_to_int
          |> st ~mode:m3 ~relative_base code (ip + 3);
          inner inputs outputs relative_base (ip + 4)
      | 8, m1, m2, m3 ->
          ld ~mode:m1 ~relative_base code (ip + 1)
          = ld ~mode:m2 ~relative_base code (ip + 2)
          |> bool_to_int
          |> st ~mode:m3 ~relative_base code (ip + 3);
          inner inputs outputs relative_base (ip + 4)
      | 9, m, _, _ ->
          let relative_base =
            relative_base + ld ~mode:m ~relative_base code (ip + 1)
          in
          inner inputs outputs relative_base (ip + 2)
      | 99, _, _, _ -> Exit { exit_code = code.(0); outputs = List.rev outputs }
      | _ -> failwith ("Unknown code: " ^ Int.to_string code.(ip))
    in
    Option.iter value0 ~f:(fun v -> code.(0) <- v);
    Option.iter noun ~f:(fun v -> code.(1) <- v);
    Option.iter verb ~f:(fun v -> code.(2) <- v);
    inner inputs [] relative_base ip

  let read ?(len = None) (input : string) : int Array.t =
    let code = String.split input ~on:',' |> List.map ~f:Int.of_string in
    let init = Array.create ~len:(Option.value len ~default:5000) 0 in
    List.foldi code ~init ~f:(fun i code x ->
        code.(i) <- x;
        code)
end

module Coordinates = struct
  module T = struct
    type t = int * int [@@deriving sexp, compare, hash]
  end

  include T
  include Comparable.Make (T)

  let start = (0, 0)
end

module Direction = struct
  type t = North [@value 1] | South | West | East [@@deriving enum, enumerate]

  let next = function
    | North -> East
    | East -> South
    | South -> West
    | West -> North

  let back = function
    | North -> South
    | South -> North
    | East -> West
    | West -> East

  let move (x, y) = function
    | North -> (x, y + 1)
    | South -> (x, y - 1)
    | West -> (x - 1, y)
    | East -> (x + 1, y)
end

module Status = struct
  type t = Wall | Move | Oxygen [@@deriving enum]
end

let go_back world position =
  Direction.all
  |> List.map ~f:(Direction.move position)
  |> List.for_all ~f:(Hashtbl.mem world)

let explore code =
  let rec backtrack ~ip ~relative_base world ~(c : Coordinates.t) = function
    | [] -> world
    | d :: ds -> (
        let d = Direction.back d in
        match
          IntCode.run code ~ip ~relative_base ~inputs:[ Direction.to_enum d ]
        with
        | IntCode.Exit _ -> failwith "Droid IntCode should not exit"
        | IntCode.Suspended { ip; relative_base; outputs = [ 1 ] } ->
            loop ~ip ~relative_base world ~c:(Direction.move c d) ~d ds
        | IntCode.Suspended { outputs; _ } ->
            failwith
              ("Unexpected outputs:" ^ List.to_string outputs ~f:Int.to_string)
        )
  and continue_explore ~ip ~relative_base world ~(c : Coordinates.t)
      ~(d : Direction.t) (path : Direction.t list) =
    match
      IntCode.run code ~ip ~relative_base ~inputs:[ Direction.to_enum d ]
    with
    | IntCode.Exit _ -> failwith "Droid IntCode should not exit"
    | IntCode.Suspended { ip; relative_base; outputs = [ data ] } -> (
        let c_new = Direction.move c d in
        Hashtbl.add_exn world ~key:c_new ~data;
        match Status.of_enum data |> Option.value_exn with
        | Wall -> loop ~ip ~relative_base world ~c ~d:(Direction.next d) path
        | Move ->
            loop ~ip ~relative_base world ~c:(Direction.move c d) ~d (d :: path)
        | Oxygen ->
            loop ~ip ~relative_base world ~c:(Direction.move c d) ~d (d :: path)
        )
    | IntCode.Suspended { outputs; _ } ->
        failwith
          ("Unexpected outputs:" ^ List.to_string outputs ~f:Int.to_string)
  and loop ~ip ~relative_base world ~(c : Coordinates.t) ~(d : Direction.t)
      (path : Direction.t list) =
    if go_back world c then backtrack ~ip ~relative_base world ~c path
    else if Hashtbl.mem world (Direction.move c d) then
      loop ~ip ~relative_base world ~c ~d:(Direction.next d) path
    else continue_explore ~ip ~relative_base world ~c ~d path
  in
  let world = Hashtbl.create (module Coordinates) in
  Hashtbl.add_exn world ~key:Coordinates.start
    ~data:(Status.to_enum Status.Move);
  loop ~ip:0 ~relative_base:0 world ~c:(0, 0) ~d:Direction.North []

let bfs ~init ~acc ~f ~next =
  let q = Queue.create () in
  let rec loop q visited acc =
    match Queue.dequeue q with
    | None -> acc
    | Some x ->
        let q, visited = next q visited x in
        loop q visited (f acc x)
  in
  Queue.enqueue q init;
  let visited = Set.empty (module Coordinates) in
  loop q visited acc

let bfs_next world q visited ((c, level) : Coordinates.t * int) =
  Direction.all
  |> List.map ~f:(Direction.move c)
  |> List.filter ~f:(fun c -> Set.mem visited c |> not)
  |> List.fold ~init:(q, visited) ~f:(fun (q, visited) c ->
         match Hashtbl.find world c |> Option.bind ~f:Status.of_enum with
         | None -> (q, visited)
         | Some Wall -> (q, Set.add visited c)
         | Some Move | Some Oxygen ->
             Queue.enqueue q (c, level + 1);
             (q, Set.add visited c))

let bfs_find_oxygen world min_level (c, level) =
  if Hashtbl.find_exn world c = Status.to_enum Oxygen then level else min_level

let bfs_fill_oxygen _ max_level (_, level) = Int.max level max_level

let shortest_path_oxygen world =
  bfs ~init:(Coordinates.start, 0) ~acc:0 ~f:(bfs_find_oxygen world)
    ~next:(bfs_next world)

let fill_oxygen world =
  let oxygen =
    Hashtbl.fold world ~init:Coordinates.start ~f:(fun ~key ~data acc ->
        if data = Status.to_enum Oxygen then key else acc)
  in
  bfs ~init:(oxygen, 0) ~acc:0 ~f:(bfs_fill_oxygen world) ~next:(bfs_next world)

let () =
  let code =
    In_channel.read_all "./input.txt" |> String.strip |> IntCode.read
  in
  let world = explore (Array.copy code) in
  shortest_path_oxygen world |> Printf.printf "part 1: %i\n";
  fill_oxygen world |> Printf.printf "part 2: %i\n"
