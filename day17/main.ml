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
  type t = North | South | West | East [@@deriving enumerate]

  let move_absolute (x, y) = function
    | North -> (x, y - 1)
    | South -> (x, y + 1)
    | West -> (x - 1, y)
    | East -> (x + 1, y)
end

module Turn = struct
  type t = Left | Right

  let to_string = function Left -> "L" | Right -> "R"

  let move_relative (x, y) (d : Direction.t) (turn : t option) =
    match (d, turn) with
    | North, Some Left -> (x - 1, y)
    | North, Some Right -> (x + 1, y)
    | South, Some Left -> (x + 1, y)
    | South, Some Right -> (x - 1, y)
    | West, Some Left -> (x, y + 1)
    | West, Some Right -> (x, y - 1)
    | East, Some Left -> (x, y - 1)
    | East, Some Right -> (x, y + 1)
    | d, None -> Direction.move_absolute (x, y) d

  let turn (d : Direction.t) (turn : t) =
    match (d, turn) with
    | North, Left -> Direction.West
    | North, Right -> East
    | South, Left -> East
    | South, Right -> West
    | West, Left -> South
    | West, Right -> North
    | East, Left -> North
    | East, Right -> South
end

let rec show = function
  | [] -> ()
  | n :: tl
    when n = 10 || n = 35 || n = 46 || n = 60 || n = 62 || n = 94 || n = 118 ->
      Printf.printf "%c" (Char.of_int_exn n);
      show tl
  | ns -> failwith ("Unexpected output: " ^ List.to_string ns ~f:Int.to_string)

let to_hashtbl xs =
  List.fold xs
    ~init:(Hashtbl.create (module Coordinates), Coordinates.start)
    ~f:(fun (h, (x, y)) -> function
      | n when n = 35 || n = 46 || n = 60 || n = 62 || n = 94 || n = 118 ->
          Hashtbl.add_exn h ~key:(x, y) ~data:n;
          (h, (x + 1, y)) | n when n = 10 -> (h, (0, y + 1))
      | n -> failwith ("Unexpected output: " ^ Int.to_string n))
  |> fst

let calibrate world =
  let is_intersection world =
    let intersections_pattern =
      List.init 4 ~f:(fun _ -> Some (Char.to_int '#'))
    in
    fun c ->
      List.map Direction.all ~f:(Direction.move_absolute c)
      |> List.map ~f:(Hashtbl.find world)
      |> List.equal (Option.equal Int.equal) intersections_pattern
  in
  Hashtbl.fold world ~init:[] ~f:(fun ~key ~data acc ->
      match Char.of_int_exn data with
      | '#' -> if is_intersection world key then key :: acc else acc
      | _ -> acc)
  |> List.fold ~init:0 ~f:(fun acc (x, y) -> (x * y) + acc)

let dry_run code =
  match IntCode.run code ~ip:0 ~relative_base:0 ~inputs:[] with
  | IntCode.Exit { outputs; _ } -> outputs
  | IntCode.Suspended _ -> failwith "Unexpected Suspended"

let greedy_forward world =
  let start_coordinates world =
    Hashtbl.fold world ~init:(Coordinates.start, Direction.North)
      ~f:(fun ~key ~data acc ->
        match Char.of_int_exn data with
        | '^' -> (key, Direction.North)
        | 'v' -> (key, Direction.South)
        | '<' -> (key, Direction.West)
        | '>' -> (key, Direction.East)
        | _ -> acc)
  in
  let is_end world c d =
    let all = [ Some Turn.Left; None; Some Right ] in
    List.map all ~f:(Turn.move_relative c d)
    |> List.map ~f:(Hashtbl.find world)
    |> List.for_all ~f:(function
         | Some n when n = Char.to_int '#' -> false
         | _ -> true)
  in
  let go_foward world c d =
    let rec loop n c =
      let next_c = Turn.move_relative c d None in
      match Hashtbl.find world next_c with
      | Some x when Char.to_int '#' = x -> loop (n + 1) next_c
      | _ -> (n, c)
    in
    loop 0 c
  in
  let next_turn c d =
    match Hashtbl.find world (Turn.move_relative c d (Some Left)) with
    | Some n when Char.to_int '#' = n -> Turn.Left
    | _ -> Right
  in
  let rec loop ~acc ~c ~d =
    if is_end world c d then acc
    else
      let n, c = go_foward world c d in
      if n = 0 then
        let turn = next_turn c d in
        loop ~acc:(acc ^ Turn.to_string turn ^ ",") ~c ~d:(Turn.turn d turn)
      else loop ~acc:(acc ^ Int.to_string n ^ ",") ~c ~d
  in
  let c, d = start_coordinates world in
  loop ~acc:"" ~c ~d

let clean_dust code =
  (* manually solved *)
  let a = "L,10,R,10,L,10,L,10\n" in
  (* manually solved *)
  let b = "R,10,R,12,L,12\n" in
  (* manually solved *)
  let c = "R,12,L,12,R,6\n" in
  (* manually solved *)
  let main_routine = "A,B,A,B,C,C,B,A,B,C\n" in
  let video_feed = "n\n" in
  let inputs =
    main_routine ^ a ^ b ^ c ^ video_feed
    |> String.to_list |> List.map ~f:Char.to_int
  in
  match IntCode.run ~value0:(Some 2) code ~ip:0 ~relative_base:0 ~inputs with
  | IntCode.Exit { outputs; _ } -> outputs
  | IntCode.Suspended _ -> failwith "Unexpected Suspended"

let () =
  let code =
    In_channel.read_all "./input.txt"
    |> String.strip
    |> IntCode.read ~len:(Some 10_000)
  in
  let world = dry_run (Array.copy code) in
  (* show world; *)
  let world = to_hashtbl world in
  calibrate world |> Printf.printf "part 1: %i\n";
  (* greedy_forward world |> Printf.printf "path: %s"; *)
  clean_dust (Array.copy code) |> List.last_exn |> Printf.printf "part 2: %i\n"
