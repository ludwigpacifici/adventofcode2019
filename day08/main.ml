open Core

let width = 25

let heigh = 6

let checksum (input : char list list) =
  List.fold input ~init:(Int.max_value, [])
    ~f:(fun (zero_count_min, layer_0_min) l ->
      let zero_count = List.count l ~f:(Char.equal '0') in
      if zero_count < zero_count_min then (zero_count, l)
      else (zero_count_min, layer_0_min))
  |> snd
  |> fun l ->
  List.count l ~f:(Char.equal '1') * List.count l ~f:(Char.equal '2')

let render (input : char list list) =
  List.fold input ~init:(List.hd_exn input) ~f:(fun top bottom ->
      List.fold2_exn top bottom ~init:[] ~f:(fun combined t b ->
          (match (t, b) with '2', b -> b | t, _ -> t) :: combined)
      |> List.rev)
  |> List.map ~f:(function '1' -> '#' | _ -> ' ')
  |> List.iteri ~f:(fun i c ->
         if i % width = 0 then Printf.printf "\n";
         Printf.printf "%c" c)

let () =
  let input =
    In_channel.read_all "./input.txt"
    |> String.strip |> String.to_list
    |> List.chunks_of ~length:(width * heigh)
  in
  checksum input |> Printf.printf "part 1:%i\n";
  Printf.printf "part 2:\n";
  render input
