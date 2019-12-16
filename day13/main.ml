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

let read_outputs o ~init ~f =
  List.chunks_of o ~length:3 |> List.fold ~init ~f:(fun acc x -> f acc x)

let count_block count = function
  | [ _; _; 2 ] -> count + 1
  | [ _; _; 0 ] | [ _; _; 1 ] | [ _; _; 3 ] | [ _; _; 4 ] | [ -1; 0; _ ] ->
      count
  | l -> failwith ("Cannot read outputs: " ^ List.to_string l ~f:Int.to_string)

let dry_run code =
  match IntCode.run code with
  | Exit { outputs; _ } -> read_outputs outputs ~init:0 ~f:count_block
  | Suspended _ -> failwith "Unexpected Suspended"

let read_score score = function [ -1; 0; s ] -> s | _ -> score

let move_paddle (paddle, ball) = function
  | [ paddle; _; 3 ] -> (paddle, ball)
  | [ ball; _; 4 ] -> (paddle, ball)
  | [ -1; 0; _ ] | [ _; _; 0 ] | [ _; _; 1 ] | [ _; _; 2 ] -> (paddle, ball)
  | l -> failwith ("Cannot read outputs: " ^ List.to_string l ~f:Int.to_string)

let play code =
  let rec loop ~ip ~relative_base ~value0 ~inputs =
    match IntCode.run ~value0 ~ip ~relative_base ~inputs code with
    | Exit { outputs; _ } -> read_outputs outputs ~init:0 ~f:read_score
    | Suspended { outputs; ip; relative_base } ->
        let paddle, ball = read_outputs outputs ~init:(0, 0) ~f:move_paddle in
        let move = Int.compare ball paddle in
        loop ~ip ~relative_base ~value0:None ~inputs:[ move ]
  in
  loop ~ip:0 ~relative_base:0 ~value0:(Some 2) ~inputs:[]

let () =
  let code =
    In_channel.read_all "./input.txt" |> String.strip |> IntCode.read
  in
  dry_run (Array.copy code) |> Printf.printf "part 1: %i\n";
  play (Array.copy code) |> Printf.printf "part 2: %i\n"
