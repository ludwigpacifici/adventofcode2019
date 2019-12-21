open Core

let read_input ~n s =
  let duplicate ~n s =
    let s_len = Array.length s in
    Array.init (n * s_len) ~f:(fun i -> s.(i % s_len))
  in
  String.to_array s |> Array.map ~f:Char.get_digit_exn |> duplicate ~n

let run_phase ~phase stepi : int =
  let phase_len = Array.length phase in
  let flip i = if i = 1 then -1 else 1 in
  let rec loop ~acc ~i ~b0 step =
    match (i, step) with
    | i, _ when i >= phase_len -> acc
    | i, 0 -> loop ~acc ~i:(i + stepi) ~b0:(flip b0) stepi
    | i, step ->
        let acc = (b0 * phase.(i)) + acc in
        loop ~acc ~i:(i + 1) ~b0 (step - 1)
  in
  loop ~acc:0 ~i:(stepi - 1) ~b0:1 stepi |> Int.abs |> fun n -> n % 10

let fft phase =
  let len = Array.length phase in
  let next_phase = Array.create ~len 0 in
  for i = 1 to len do
    next_phase.(i - 1) <- run_phase ~phase i
  done;
  next_phase

let fft_cheat phase =
  (* Not fun problem, cheat explanation here: https://github.com/mebeim/aoc/blob/master/2019/README.md *)
  let sum = ref 0 in
  for i = Array.length phase - 1 downto 0 do
    sum := !sum + phase.(i);
    phase.(i) <- !sum % 10
  done;
  phase

let () =
  let input = In_channel.read_all "./input.txt" |> String.strip in
  let arr = read_input ~n:1 input in
  Fn.apply_n_times ~n:100 fft arr
  |> Array.sub ~pos:0 ~len:8
  |> Array.fold ~init:0 ~f:(fun acc n -> n + (acc * 10))
  |> Printf.printf "part1: %i\n";

  let offset = String.sub input ~pos:0 ~len:7 |> Int.of_string in
  let arr = read_input ~n:10_000 input in
  let arr = Array.sub arr ~pos:offset ~len:(Array.length arr - offset) in
  Fn.apply_n_times ~n:100 fft_cheat arr
  |> Array.sub ~pos:0 ~len:8
  |> Array.fold ~init:0 ~f:(fun acc n -> n + (acc * 10))
  |> Printf.printf "part2: %i\n"
