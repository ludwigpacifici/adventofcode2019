open Core

module Coordinates = struct
  type t = { x : int; y : int; z : int } [@@deriving eq]

  let project_x { x; _ } = { x; y = 0; z = 0 }

  let project_y { y; _ } = { x = 0; y; z = 0 }

  let project_z { z; _ } = { x = 0; y = 0; z }

  let of_tuple3 (x, y, z) = { x; y; z }

  let zeros = { x = 0; y = 0; z = 0 }

  let fold { x; y; z } ~init ~f = f (f (f init x) y) z

  let map c ~f = { x = f c.x; y = f c.y; z = f c.z }

  let map_all c0 c1 ~f = { x = f c0.x c1.x; y = f c0.y c1.y; z = f c0.z c1.z }

  let compare c0 c1 = map_all c0 c1 ~f:Int.compare
end

module Moon = struct
  type t = { position : Coordinates.t; velocity : Coordinates.t }
  [@@deriving eq]

  let of_position position = { position; velocity = Coordinates.zeros }

  let potential_energy { position; _ } =
    Coordinates.map position ~f:Int.abs |> Coordinates.fold ~init:0 ~f:( + )

  let kinetic_energy { velocity; _ } =
    Coordinates.map velocity ~f:Int.abs |> Coordinates.fold ~init:0 ~f:( + )

  let total_energy moon = potential_energy moon * kinetic_energy moon

  let gravity { position; velocity } p =
    let dp = Coordinates.compare p position in
    { position; velocity = Coordinates.map_all velocity dp ~f:( + ) }

  let move { position; velocity } =
    { position = Coordinates.map_all position velocity ~f:( + ); velocity }
end

let moons l =
  Array.map l ~f:Coordinates.of_tuple3 |> Array.map ~f:Moon.of_position

let step moons =
  let last = Array.length moons - 1 in
  for i = 0 to last do
    for j = 0 to last do
      if Int.equal i j |> not then
        moons.(i) <- Moon.gravity moons.(i) moons.(j).position
    done
  done;
  Array.map moons ~f:Moon.move

let energy moons ~f = moons |> Array.map ~f |> Array.fold ~init:0 ~f:( + )

let energy_at ~n moons =
  List.init n ~f:Fn.id
  |> List.fold ~init:(Array.copy moons) ~f:(fun moons _ -> step moons)
  |> energy ~f:Moon.total_energy

let repeat_itself moons =
  let ms0 = Array.copy moons in
  let rec loop moons n =
    let moons = step moons in
    if Array.equal Moon.equal ms0 moons then n else loop moons (n + 1)
  in
  loop (Array.copy moons) 1

let rec gcd x y = if y = 0 then abs x else gcd y (x % y)

let lcm x y =
  match (x, y) with 0, _ | _, 0 -> 0 | x, y -> abs (x * y) / gcd x y

let repeat_history (moons : Moon.t Array.t) =
  [ Coordinates.project_x; Coordinates.project_y; Coordinates.project_z ]
  |> List.fold ~init:[] ~f:(fun acc f ->
         ( Array.map moons ~f:(fun m -> { m with position = f m.position })
         |> repeat_itself )
         :: acc)
  |> List.reduce_exn ~f:lcm

let () =
  let input = moons [| (3, 3, 0); (4, -16, 2); (-10, -6, 5); (-3, 0, -13) |] in
  energy_at ~n:1000 input |> Printf.printf "part1: %i\n";
  repeat_history input |> Printf.printf "part2: %i\n"
