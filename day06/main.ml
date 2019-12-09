open Core

let parse_relation x =
  match String.split x ~on:')' with
  | [ a; b ] -> (a, b)
  | _ -> failwith ("Unknown input: " ^ x)

let make (input : string list) =
  List.map input ~f:parse_relation
  |> List.fold
       ~init:(Map.empty (module String))
       ~f:(fun m (p, c) ->
         Map.update m p ~f:(fun o ->
             Option.value o ~default:[] |> fun l -> c :: l))

let count orbits : int =
  let rec inorder_traversal acc = function
    | [] -> acc
    | (o, level) :: to_visit ->
        let children =
          Map.find orbits o |> Option.value ~default:[]
          |> List.map ~f:(fun o -> (o, level + 1))
        in
        inorder_traversal (level + acc) (children @ to_visit)
  in
  inorder_traversal 0 [ ("com", 0) ]

let rec path orbits target com =
  if String.equal com target then [ target ]
  else
    let children = Map.find orbits com |> Option.value ~default:[] in
    List.fold_until children ~init:[]
      ~f:(fun acc c ->
        match path orbits target c with
        | [] -> Continue_or_stop.Continue acc
        | path -> Continue_or_stop.Continue (com :: path))
      ~finish:Fn.id

let orbital_transfer orbits start stop =
  let rec fold2 acc p1 p2 =
    match (p1, p2) with
    | o1 :: p1, o2 :: p2 when String.equal o1 o2 -> fold2 (acc + 1) p1 p2
    | [], _ | _, [] -> failwith "Found sub-path"
    | _, _ -> acc
  in
  let p1 = path orbits start "com" in
  let p2 = path orbits stop "com" in
  let common_path_len = fold2 0 p1 p2 in
  List.length p1 + List.length p2 - (2 * (common_path_len + 1))

let () =
  let orbits =
    In_channel.read_lines "./input.txt"
    |> List.map ~f:String.strip
    |> List.map ~f:String.lowercase
    |> make
  in
  count orbits |> Printf.printf "part 1: %i\n";
  orbital_transfer orbits "you" "san" |> Printf.printf "part 2: %i\n"
