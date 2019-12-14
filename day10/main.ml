open Core

module Coordinates = struct
  type t = float * float

  let of_cartesian (x : float) (y : float) : t = (x, y)

  let to_polar ((x, y) : t) : t =
    (Float.sqrt ((x *. x) +. (y *. y)), Float.atan2 y x)

  let move_base (x0, y0) (x, y) : t = (x -. x0, y -. y0)

  let equal (x0, y0) (x1, y1) = Float.equal x0 x1 && Float.equal y0 y1

  let not_equal c0 c1 = equal c0 c1 |> not
end

module Asteroid = struct
  type t = Empty | Asteroid

  let of_char = function
    | '.' -> Empty
    | '#' -> Asteroid
    | c -> failwith ("Unknown input: " ^ Char.to_string c)

  let map_or ~default ~f = function Empty -> default | Asteroid -> f ()
end

module Galaxy = struct
  type t = Asteroid.t Array.t Array.t

  let of_string (s : string) : t =
    String.split_lines s |> List.map ~f:String.strip
    |> List.map ~f:String.to_array
    |> List.map ~f:(Array.map ~f:Asteroid.of_char)
    |> List.to_array

  let fold (arr : t) ~init
      ~(f : Coordinates.t -> 'accum -> Asteroid.t -> 'accum) =
    Array.foldi arr ~init ~f:(fun y acc line ->
        Array.foldi line ~init:acc ~f:(fun x acc asteroid ->
            f (Float.of_int x, Float.of_int y) acc asteroid))

  let start = Coordinates.of_cartesian 0. 0.
end

let empty_view = Map.empty (module Float)

let relative_view (g : Galaxy.t) (station : Coordinates.t) =
  Galaxy.fold g ~init:empty_view ~f:(fun position phi_radiuses a ->
      if Coordinates.not_equal station position then
        Asteroid.map_or a ~default:phi_radiuses ~f:(fun () ->
            let r, phi =
              Coordinates.move_base station position |> Coordinates.to_polar
            in
            Map.update phi_radiuses phi ~f:(fun rs ->
                Option.value rs ~default:[] |> List.cons (r, position)))
      else phi_radiuses)
  |> Map.map ~f:(fun l ->
         List.sort l ~compare:(fun x y -> Float.compare (fst x) (fst y))
         |> List.map ~f:snd)

let position_count (g : Galaxy.t) (station : Coordinates.t) best_view () =
  let view = relative_view g station in
  if Map.length view > Map.length best_view then view else best_view

let best_location (g : Galaxy.t) =
  Galaxy.fold g ~init:empty_view ~f:(fun station view a ->
      Asteroid.map_or a ~default:view ~f:(position_count g station view))

let vaporization view ~n =
  let start, _ =
    Option.value_exn (Map.closest_key view `Less_than (-.Float.pi /. 2.))
  in
  let rec loop (x, y) start view = function
    | 0 -> (x *. 100.) +. y |> Int.of_float
    | n when n > 0 -> (
        match Map.closest_key view `Greater_than start with
        | None -> loop (x, y) (-.Float.pi) view n
        | Some (phi, cs) -> (
            match cs with
            | [] -> failwith (Float.to_string phi ^ "key has no data")
            | [ c ] -> loop c phi (Map.remove view phi) (n - 1)
            | c :: cs -> loop c phi (Map.set view ~key:phi ~data:cs) (n - 1) ) )
    | n -> failwith ("Looping too many times: " ^ Int.to_string n)
  in
  loop Galaxy.start start view n

let () =
  let g = In_channel.read_all "./input.txt" |> Galaxy.of_string in
  let view = best_location g in
  Map.length view |> Printf.printf "part 1: %i\n";
  vaporization view ~n:200 |> Printf.printf "part 2: %i\n"
