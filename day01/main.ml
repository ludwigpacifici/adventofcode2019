open Core

let mass_to_fuel m = (m / 3) - 2

let fuel_for_fuel m =
  let rec inner acc m =
    match mass_to_fuel m with m when m <= 0 -> acc | m -> inner (acc + m) m
  in
  inner 0 m

let fuel (input : string list) : int =
  List.map input ~f:Int.of_string
  |> List.map ~f:mass_to_fuel |> List.fold ~init:0 ~f:( + )

let fuel2 (input : string list) : int =
  List.map input ~f:Int.of_string
  |> List.map ~f:mass_to_fuel
  |> List.fold ~init:0 ~f:(fun acc m -> acc + m + fuel_for_fuel m)

let () =
  let input = In_channel.read_lines "./input.txt" |> List.map ~f:String.strip in
  input |> fuel |> Printf.printf "part 1: %i\n";
  input |> fuel2 |> Printf.printf "part 2: %i\n"
