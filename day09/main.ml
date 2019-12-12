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

let run (code : int Array.t) ~(noun : int option) ~(verb : int option)
    ~(input : int) =
  let rec inner relative_base ip =
    match read_instruction code.(ip) with
    | 1, m1, m2, m3 ->
        ld ~mode:m1 ~relative_base code (ip + 1)
        + ld ~mode:m2 ~relative_base code (ip + 2)
        |> st ~mode:m3 ~relative_base code (ip + 3);
        inner relative_base (ip + 4)
    | 2, m1, m2, m3 ->
        ld ~mode:m1 ~relative_base code (ip + 1)
        * ld ~mode:m2 ~relative_base code (ip + 2)
        |> st ~mode:m3 ~relative_base code (ip + 3);
        inner relative_base (ip + 4)
    | 3, m, _, _ when m = 0 || m = 2 ->
        st ~mode:m ~relative_base code (ip + 1) input;
        inner relative_base (ip + 2)
    | 4, m, _, _ ->
        ld ~mode:m ~relative_base code (ip + 1) |> Printf.printf "%i\n";
        inner relative_base (ip + 2)
    | 5, m1, m2, _ ->
        if ld ~mode:m1 ~relative_base code (ip + 1) <> 0 then
          ld ~mode:m2 ~relative_base code (ip + 2) |> inner relative_base
        else inner relative_base (ip + 3)
    | 6, m1, m2, _ ->
        if ld ~mode:m1 ~relative_base code (ip + 1) = 0 then
          ld ~mode:m2 ~relative_base code (ip + 2) |> inner relative_base
        else inner relative_base (ip + 3)
    | 7, m1, m2, m3 ->
        ld ~mode:m1 ~relative_base code (ip + 1)
        < ld ~mode:m2 ~relative_base code (ip + 2)
        |> bool_to_int
        |> st ~mode:m3 ~relative_base code (ip + 3);
        inner relative_base (ip + 4)
    | 8, m1, m2, m3 ->
        ld ~mode:m1 ~relative_base code (ip + 1)
        = ld ~mode:m2 ~relative_base code (ip + 2)
        |> bool_to_int
        |> st ~mode:m3 ~relative_base code (ip + 3);
        inner relative_base (ip + 4)
    | 9, m, _, _ ->
        let relative_base =
          relative_base + ld ~mode:m ~relative_base code (ip + 1)
        in
        inner relative_base (ip + 2)
    | 99, _, _, _ -> code.(0)
    | _ -> failwith ("Unknown code: " ^ Int.to_string code.(ip))
  in
  Option.iter noun ~f:(fun v -> code.(1) <- v);
  Option.iter verb ~f:(fun v -> code.(2) <- v);
  inner 0 0

let parse_code ~len (input : string) : int Array.t =
  let code = String.split input ~on:',' |> List.map ~f:Int.of_string in
  List.foldi code ~init:(Array.create ~len 0) ~f:(fun i code x ->
      code.(i) <- x;
      code)

let () =
  let code =
    In_channel.read_all "./input.txt" |> String.strip |> parse_code ~len:1_077
  in
  Printf.printf "part 1:\n";
  run (Array.copy code) ~noun:None ~verb:None ~input:1 |> ignore;
  Printf.printf "part 2:\n";
  run (Array.copy code) ~noun:None ~verb:None ~input:2 |> ignore
