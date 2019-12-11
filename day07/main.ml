open Core

let bool_to_int (x : bool) : int = if x then 1 else 0

let ld ~mode arr i =
  match mode with
  | 0 -> arr.(arr.(i))
  | 1 -> arr.(i)
  | n -> failwith ("Unknown ld mode: " ^ Int.to_string n)

let st ~mode arr i value =
  match mode with
  | 0 -> arr.(arr.(i)) <- value
  | 1 -> arr.(i) <- value
  | n -> failwith ("Unknown st mode: " ^ Int.to_string n)

let read_instruction n =
  (n mod 100, n / 100 mod 10, n / 1_000 mod 10, n / 10_000 mod 10)

let run ~(code : int Array.t) ~(noun : int option) ~(verb : int option)
    ~(inputs : int list) ~(ip : int) : int option * int list * int =
  let rec inner inputs outputs ip =
    match read_instruction code.(ip) with
    | 1, m1, m2, _ ->
        ld ~mode:m1 code (ip + 1) + ld ~mode:m2 code (ip + 2)
        |> st ~mode:0 code (ip + 3);
        inner inputs outputs (ip + 4)
    | 2, m1, m2, _ ->
        ld ~mode:m1 code (ip + 1) * ld ~mode:m2 code (ip + 2)
        |> st ~mode:0 code (ip + 3);
        inner inputs outputs (ip + 4)
    | 3, 0, _, _ -> (
        match inputs with
        | [] -> (None, List.rev outputs, ip)
        | i :: inputs ->
            st ~mode:0 code (ip + 1) i;
            inner inputs outputs (ip + 2) )
    | 4, m, _, _ ->
        let out = ld ~mode:m code (ip + 1) in
        Printf.printf "%i\n" out;
        inner inputs (out :: outputs) (ip + 2)
    | 5, m1, m2, _ ->
        if ld ~mode:m1 code (ip + 1) <> 0 then
          ld ~mode:m2 code (ip + 2) |> inner inputs outputs
        else inner inputs outputs (ip + 3)
    | 6, m1, m2, _ ->
        if ld ~mode:m1 code (ip + 1) = 0 then
          ld ~mode:m2 code (ip + 2) |> inner inputs outputs
        else inner inputs outputs (ip + 3)
    | 7, m1, m2, _ ->
        ld ~mode:m1 code (ip + 1)
        < ld ~mode:m2 code (ip + 2)
        |> bool_to_int
        |> st ~mode:0 code (ip + 3);
        inner inputs outputs (ip + 4)
    | 8, m1, m2, _ ->
        ld ~mode:m1 code (ip + 1)
        = ld ~mode:m2 code (ip + 2)
        |> bool_to_int
        |> st ~mode:0 code (ip + 3);
        inner inputs outputs (ip + 4)
    | 99, _, _, _ -> (Some code.(0), List.rev outputs, ip)
    | _ -> failwith ("Unknown code: " ^ Int.to_string code.(ip))
  in
  Option.iter noun ~f:(fun v -> code.(1) <- v);
  Option.iter verb ~f:(fun v -> code.(2) <- v);
  inner inputs [] ip

let run_once = run ~noun:None ~verb:None

let parse_code (input : string) : int Array.t =
  let code = String.split input ~on:',' |> List.map ~f:Int.of_string in
  List.foldi code
    ~init:(Array.create ~len:(List.length code) 0)
    ~f:(fun i code x ->
      code.(i) <- x;
      code)

let rec combinaisons = function
  | [] -> []
  | [ x ] -> [ [ x ] ]
  | xs ->
      let rm xs x = List.filter xs ~f:(fun y -> Int.equal x y |> not) in
      List.fold xs ~init:[] ~f:(fun acc x ->
          List.map (combinaisons (rm xs x)) ~f:(fun l -> x :: l) @ acc)

module Amplifiers = struct
  type t = { code : int Array.t; ip : int ref; inputs : int list ref }

  let length = 5

  let make code sequence =
    Array.init length ~f:(fun i ->
        if i = 0 then
          {
            code = Array.copy code;
            ip = ref 0;
            inputs = ref [ List.nth_exn sequence i; 0 ];
          }
        else
          {
            code = Array.copy code;
            ip = ref 0;
            inputs = ref [ List.nth_exn sequence i ];
          })

  let next i = (i + 1) % length

  let context_switch amplifiers i ~ip ~signals =
    amplifiers.(i).ip := ip;
    amplifiers.(i).inputs := [];
    let j = next i in
    amplifiers.(j).inputs := !(amplifiers.(j).inputs) @ signals;
    j
end

let feedback_loop code sequence =
  let amplifiers = Amplifiers.make code sequence in
  let rec loop i =
    match
      run_once ~code:amplifiers.(i).code ~inputs:!(amplifiers.(i).inputs)
        ~ip:!(amplifiers.(i).ip)
    with
    | Some _, signals, _ when i = Amplifiers.length - 1 -> List.last_exn signals
    | _, signals, ip ->
        Amplifiers.context_switch amplifiers i ~ip ~signals |> loop
  in
  loop 0

let amplifiers ~phases ~compute code =
  combinaisons phases
  |> List.fold ~init:0 ~f:(fun max_thruster sequence ->
         max (compute code sequence) max_thruster)

let () =
  let code = In_channel.read_all "./input.txt" |> String.strip |> parse_code in
  amplifiers ~phases:[ 0; 1; 2; 3; 4 ] ~compute:feedback_loop code
  |> Printf.printf "part 1:%i\n";
  let code = In_channel.read_all "./input.txt" |> String.strip |> parse_code in
  amplifiers ~phases:[ 5; 6; 7; 8; 9 ] ~compute:feedback_loop code
  |> Printf.printf "part 2:%i\n"
