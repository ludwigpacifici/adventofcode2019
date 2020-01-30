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

let to_ascii_script str =
  String.fold str ~init:[] ~f:(fun acc c -> Char.to_int c :: acc) |> List.rev

let to_str outputs = List.map outputs ~f:Char.of_int_exn |> String.of_char_list

let rec interactive ?(inputs = []) ?(ip = 0) ?(relative_base = 0) code =
  match IntCode.run code ~inputs ~ip ~relative_base with
  | Exit { exit_code; outputs } ->
      Printf.printf "[Exit] outputs: %s\nExit code:%i\n" (to_str outputs)
        exit_code
  | Suspended { outputs; ip; relative_base } ->
      to_str outputs |> Printf.printf "%s\n> ";
      let inputs = Caml.read_line () ^ "\n" |> to_ascii_script in
      interactive ~inputs ~ip ~relative_base code

let () =
  let code =
    In_channel.read_all "./input.txt"
    |> String.strip
    |> IntCode.read ~len:(Some 20_000)
  in
  interactive code

(*
  take antenna (Crew Quarters)
  take weather machine (Hallway)
  take spool of cat6 (Stables)
  take mug (Kitchen)
  exit: Hall Breach -> Hallway -> Storage -> Stables -> Warp Drive Maintenance -> Security Check Point
*)
