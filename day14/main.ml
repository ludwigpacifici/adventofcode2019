open Core

module Component = struct
  type t = { n : int; chemical : string }

  let of_string s =
    match String.split s ~on:' ' with
    | [ n; chemical ] -> { n = Int.of_string n; chemical }
    | _ -> failwith ("Unknown input: " ^ s)
end

module Reaction = struct
  type t = { inputs : Component.t list; output : Component.t }

  let split_arrow s =
    let rec loop acc = function
      | [] -> (s, "")
      | a :: b :: tl when Char.equal a '=' && Char.equal b '>' ->
          ( List.rev acc |> String.of_char_list |> String.strip,
            String.of_char_list tl |> String.strip )
      | a :: tl -> loop (a :: acc) tl
    in
    loop [] (String.to_list s)

  let of_string s =
    let inputs, output = split_arrow s in
    let inputs =
      String.split inputs ~on:','
      |> List.map ~f:String.strip
      |> List.map ~f:Component.of_string
    in
    let output = Component.of_string output in
    { inputs; output }
end

let resolve_dependencies (r : Reaction.t list) =
  List.fold r
    ~init:(Map.empty (module String))
    ~f:(fun m ({ output; _ } as reaction) ->
      Map.add_exn m ~key:output.chemical ~data:reaction)

let multiplier ~want ~available =
  if want < available then 1
  else if want % available = 0 then want / available
  else (want / available) + 1

let consume leftovers (c : Component.t) =
  match Map.find leftovers c.chemical with
  | None -> (c.n, leftovers)
  | Some d when d <= c.n -> (c.n - d, Map.remove leftovers c.chemical)
  | Some d -> (0, Map.set leftovers ~key:c.chemical ~data:(d - c.n))

let cost ~from ~target tree : int =
  let rec loop (from_count : int) leftovers (cs : Component.t list) =
    match cs with
    | [] -> from_count
    | { n; chemical } :: cs when String.equal from chemical ->
        loop (from_count + n) leftovers cs
    | ({ chemical; _ } as c) :: cs -> (
        match consume leftovers c with
        | 0, leftovers -> loop from_count leftovers cs
        | n, leftovers ->
            let r : Reaction.t = Map.find_exn tree chemical in
            let m = multiplier ~want:n ~available:r.output.n in
            let residu = (m * r.output.n) - n in
            let leftovers =
              Map.update leftovers chemical ~f:(fun o ->
                  Option.value o ~default:0 |> fun x -> x + residu)
            in
            let expand =
              List.map r.inputs ~f:(fun c -> { c with n = m * c.n })
            in
            loop from_count leftovers (cs @ expand) )
  in
  loop 0 (Map.empty (module String)) [ target ]

let generate reactions ~(from : Component.t) ~target =
  let rec loop n =
    if
      cost reactions ~from:from.chemical ~target:{ n; chemical = target }
      < from.n
    then loop (n + 1)
    else n - 1
  in
  (* I am Lazy *)
  loop 5650100

let () =
  let reactions =
    In_channel.read_lines "./input.txt"
    |> List.map ~f:String.strip
    |> List.map ~f:String.lowercase
    |> List.map ~f:Reaction.of_string
    |> resolve_dependencies
  in
  cost reactions ~from:"ore" ~target:{ n = 1; chemical = "fuel" }
  |> Printf.printf "part 1:%i\n";
  generate reactions
    ~from:{ n = 1_000_000_000_000; chemical = "ore" }
    ~target:"fuel"
  |> Printf.printf "part 2:%i\n"
