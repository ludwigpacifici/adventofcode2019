open Core

let run (code : int Array.t) ~(noun : int) ~(verb : int) =
  let rec inner ip =
    match code.(ip) with
    | 1 ->
        code.(code.(ip + 3)) <- code.(code.(ip + 1)) + code.(code.(ip + 2));
        inner (ip + 4)
    | 2 ->
        code.(code.(ip + 3)) <- code.(code.(ip + 1)) * code.(code.(ip + 2));
        inner (ip + 4)
    | 99 -> code.(0)
    | code -> failwith ("Unknown code: " ^ Int.to_string code)
  in
  code.(1) <- noun;
  code.(2) <- verb;
  inner 0

let parse_code (input : string) : int Array.t =
  let code = String.split input ~on:',' |> List.map ~f:Int.of_string in
  List.foldi code
    ~init:(Array.create ~len:(List.length code) 0)
    ~f:(fun i code x ->
      code.(i) <- x;
      code)

let find_19690720 (code : int Array.t) : int =
  let rec find noun verb =
    match (run (Array.copy code) ~noun ~verb, noun, verb) with
    | 19690720, noun, verb -> (100 * noun) + verb
    | _, 100, verb -> find 0 (verb + 1)
    | _, noun, verb -> find (noun + 1) verb
  in
  find 0 0

let () =
  let code = In_channel.read_all "./input.txt" |> String.strip |> parse_code in
  run (Array.copy code) ~noun:12 ~verb:2 |> Printf.printf "part 1: %i\n";
  find_19690720 (Array.copy code) |> Printf.printf "part 2: %i\n"
