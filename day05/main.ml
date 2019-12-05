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

let run (code : int Array.t) ~(noun : int option) ~(verb : int option)
    ~(input : int) =
  let rec inner ip =
    match read_instruction code.(ip) with
    | 1, m1, m2, _ ->
        ld ~mode:m1 code (ip + 1) + ld ~mode:m2 code (ip + 2)
        |> st ~mode:0 code (ip + 3);
        inner (ip + 4)
    | 2, m1, m2, _ ->
        ld ~mode:m1 code (ip + 1) * ld ~mode:m2 code (ip + 2)
        |> st ~mode:0 code (ip + 3);
        inner (ip + 4)
    | 3, 0, _, _ ->
        st ~mode:0 code (ip + 1) input;
        inner (ip + 2)
    | 4, m, _, _ ->
        ld ~mode:m code (ip + 1) |> Printf.printf "%i\n";
        inner (ip + 2)
    | 5, m1, m2, _ ->
        if ld ~mode:m1 code (ip + 1) <> 0 then
          ld ~mode:m2 code (ip + 2) |> inner
        else inner (ip + 3)
    | 6, m1, m2, _ ->
        if ld ~mode:m1 code (ip + 1) = 0 then ld ~mode:m2 code (ip + 2) |> inner
        else inner (ip + 3)
    | 7, m1, m2, _ ->
        ld ~mode:m1 code (ip + 1)
        < ld ~mode:m2 code (ip + 2)
        |> bool_to_int
        |> st ~mode:0 code (ip + 3);
        inner (ip + 4)
    | 8, m1, m2, _ ->
        ld ~mode:m1 code (ip + 1)
        = ld ~mode:m2 code (ip + 2)
        |> bool_to_int
        |> st ~mode:0 code (ip + 3);
        inner (ip + 4)
    | 99, _, _, _ -> code.(0)
    | _ -> failwith ("Unknown code: " ^ Int.to_string code.(ip))
  in
  Option.iter noun ~f:(fun v -> code.(1) <- v);
  Option.iter verb ~f:(fun v -> code.(2) <- v);
  inner 0

let parse_code (input : string) : int Array.t =
  let code = String.split input ~on:',' |> List.map ~f:Int.of_string in
  List.foldi code
    ~init:(Array.create ~len:(List.length code) 0)
    ~f:(fun i code x ->
      code.(i) <- x;
      code)

let () =
  let code = In_channel.read_all "./input.txt" |> String.strip |> parse_code in
  Printf.printf "part 1:\n";
  run (Array.copy code) ~noun:None ~verb:None ~input:1 |> ignore;
  Printf.printf "part 2:\n";
  run (Array.copy code) ~noun:None ~verb:None ~input:5 |> ignore
