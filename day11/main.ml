open Core

let bool_to_int (x : bool) : int = if x then 1 else 0

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

type exit = { return_code : int; outputs : int list }

type suspended = { outputs : int list; ip : int; relative_base : int }

type state = Exit of exit | Suspended of suspended

let run (code : int Array.t) ~(noun : int option) ~(verb : int option)
    ~(inputs : int list) ~relative_base ~ip : state =
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
    | 99, _, _, _ -> Exit { return_code = code.(0); outputs = List.rev outputs }
    | _ -> failwith ("Unknown code: " ^ Int.to_string code.(ip))
  in
  Option.iter noun ~f:(fun v -> code.(1) <- v);
  Option.iter verb ~f:(fun v -> code.(2) <- v);
  inner inputs [] relative_base ip

let parse_code ~len (input : string) : int Array.t =
  let code = String.split input ~on:',' |> List.map ~f:Int.of_string in
  List.foldi code ~init:(Array.create ~len 0) ~f:(fun i code x ->
      code.(i) <- x;
      code)

module Coordinates = struct
  module T = struct
    type t = int * int [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)
end

type direction = Up | Down | Left | Right

let next (x, y) (turn : int) = function
  | Up when Int.equal turn 0 -> ((x - 1, y), Left)
  | Up -> ((x + 1, y), Right)
  | Down when Int.equal turn 0 -> ((x + 1, y), Right)
  | Down -> ((x - 1, y), Left)
  | Left when Int.equal turn 0 -> ((x, y - 1), Down)
  | Left -> ((x, y + 1), Up)
  | Right when Int.equal turn 0 -> ((x, y + 1), Up)
  | Right -> ((x, y - 1), Down)

let make_panel code ~color =
  let rec loop panel position direction = function
    | Exit _ -> panel
    | Suspended { outputs = color :: turns; ip; relative_base } ->
        let panel = Map.set panel ~key:position ~data:color in
        let position, direction =
          List.fold turns ~init:(position, direction)
            ~f:(fun (position, direction) turn -> next position turn direction)
        in
        let input = Map.find panel position |> Option.value ~default:0 in
        run code ~noun:None ~verb:None ~inputs:[ input ] ~relative_base ~ip
        |> loop panel position direction
    | Suspended { outputs = []; _ } -> failwith "No ouputs returned"
  in
  let panel = Map.empty (module Coordinates) in
  let state = Suspended { outputs = [ color ]; ip = 0; relative_base = 0 } in
  loop panel (0, 0) Up state

let draw panel =
  for x = 1 to 38 do
    for y = -5 to 0 do
      match Map.find panel (x, y) with
      | None -> Printf.printf "%c" ' '
      | Some n when Int.equal n 0 -> Printf.printf "%c" ' '
      | Some n when Int.equal n 1 -> Printf.printf "%c" '#'
      | Some n -> failwith ("Unknown digit color: " ^ Int.to_string n)
    done;
    Printf.printf "\n"
  done

let () =
  let code =
    In_channel.read_all "./input.txt" |> String.strip |> parse_code ~len:1_109
  in
  make_panel (Array.copy code) ~color:0
  |> Map.length
  |> Printf.printf "part 1: %i\n";
  Printf.printf "part 2:\n";
  make_panel (Array.copy code) ~color:1 |> draw
