open Core

let bool_to_int (x : bool) : int = if x then 1 else 0

module IntCode = struct
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

  type exit = { exit_code : int; outputs : int list }

  type suspended = { outputs : int list; ip : int; relative_base : int }

  type t = Exit of exit | Suspended of suspended

  let run ?(value0 = None) ?(noun = None) ?(verb = None) ?(inputs = [])
      ?(relative_base = 0) ?(ip = 0) (code : int Array.t) =
    let rec inner inputs outputs relative_base ip =
      match read_instruction code.(ip) with
      | 1, m1, m2, m3 ->
          ld ~mode:m1 ~relative_base code (ip + 1)
          + ld ~mode:m2 ~relative_base code (ip + 2)
          |> st ~mode:m3 ~relative_base code (ip + 3);
          inner inputs outputs relative_base (ip + 4)
      | 2, m1, m2, m3 ->
          ld ~mode:m1 ~relative_base code (ip + 1)
          * ld ~mode:m2 ~relative_base code (ip + 2)
          |> st ~mode:m3 ~relative_base code (ip + 3);
          inner inputs outputs relative_base (ip + 4)
      | 3, m, _, _ when m = 0 || m = 2 -> (
          match inputs with
          | [] -> Suspended { outputs = List.rev outputs; ip; relative_base }
          | i :: inputs ->
              st ~mode:m ~relative_base code (ip + 1) i;
              inner inputs outputs relative_base (ip + 2) )
      | 4, m, _, _ ->
          let out = ld ~mode:m ~relative_base code (ip + 1) in
          inner inputs (out :: outputs) relative_base (ip + 2)
      | 5, m1, m2, _ ->
          if ld ~mode:m1 ~relative_base code (ip + 1) <> 0 then
            ld ~mode:m2 ~relative_base code (ip + 2)
            |> inner inputs outputs relative_base
          else inner inputs outputs relative_base (ip + 3)
      | 6, m1, m2, _ ->
          if ld ~mode:m1 ~relative_base code (ip + 1) = 0 then
            ld ~mode:m2 ~relative_base code (ip + 2)
            |> inner inputs outputs relative_base
          else inner inputs outputs relative_base (ip + 3)
      | 7, m1, m2, m3 ->
          ld ~mode:m1 ~relative_base code (ip + 1)
          < ld ~mode:m2 ~relative_base code (ip + 2)
          |> bool_to_int
          |> st ~mode:m3 ~relative_base code (ip + 3);
          inner inputs outputs relative_base (ip + 4)
      | 8, m1, m2, m3 ->
          ld ~mode:m1 ~relative_base code (ip + 1)
          = ld ~mode:m2 ~relative_base code (ip + 2)
          |> bool_to_int
          |> st ~mode:m3 ~relative_base code (ip + 3);
          inner inputs outputs relative_base (ip + 4)
      | 9, m, _, _ ->
          let relative_base =
            relative_base + ld ~mode:m ~relative_base code (ip + 1)
          in
          inner inputs outputs relative_base (ip + 2)
      | 99, _, _, _ -> Exit { exit_code = code.(0); outputs = List.rev outputs }
      | _ -> failwith ("Unknown code: " ^ Int.to_string code.(ip))
    in
    Option.iter value0 ~f:(fun v -> code.(0) <- v);
    Option.iter noun ~f:(fun v -> code.(1) <- v);
    Option.iter verb ~f:(fun v -> code.(2) <- v);
    inner inputs [] relative_base ip

  let read ?(len = None) (input : string) : int Array.t =
    let code = String.split input ~on:',' |> List.map ~f:Int.of_string in
    let init = Array.create ~len:(Option.value len ~default:5000) 0 in
    List.foldi code ~init ~f:(fun i code x ->
        code.(i) <- x;
        code)
end

let to_tuple3_exn = function
  | [ a; b; c ] -> (a, b, c)
  | xs -> failwith ("Not a Tuple3: " ^ List.to_string xs ~f:Int.to_string)

module Network = struct
  type state = { ip : int ref; relative_base : int ref }

  type node = { code : int array; q : int Queue.t; state : state }

  type t = {
    length : int;
    node : node array;
    idle : bool ref;
    delivered_nat_y : int option ref;
    nat : (int * int) option ref;
  }

  let init ~len code =
    {
      length = len;
      node =
        Array.init len ~f:(fun _ ->
            {
              code = Array.copy code;
              q = Queue.create ();
              state = { ip = ref 0; relative_base = ref 0 };
            });
      idle = ref false;
      delivered_nat_y = ref None;
      nat = ref None;
    }

  let context_switch (s : state) ip relative_base =
    s.ip := ip;
    s.relative_base := relative_base

  let send_outputs q x y = Queue.enqueue_all q [ x; y ]

  let boot (n : t) =
    for addr = 0 to n.length - 1 do
      let computer = n.node.(addr) in
      match IntCode.run computer.code ~inputs:[ addr ] with
      | Exit _ -> failwith ("Exited: " ^ Int.to_string addr)
      | Suspended { ip; relative_base; _ } ->
          context_switch computer.state ip relative_base
    done

  let resume (n : t) =
    let f (x, y) =
      send_outputs n.node.(0).q x y;
      ( match !(n.delivered_nat_y) with
      | None -> Printf.printf "part 1: %i\n" y
      | Some previous_y when previous_y = y ->
          Printf.printf "part 2: %i\n" y;
          exit 0
      | Some _ -> () );
      n.delivered_nat_y := Some y
    in
    Option.iter !(n.nat) ~f

  let run_one (n : t) addr =
    let computer = n.node.(addr) in
    let input =
      if Queue.is_empty computer.q then -1
      else (
        n.idle := false;
        Queue.dequeue_exn computer.q )
    in
    match
      IntCode.run computer.code ~inputs:[ input ] ~ip:!(computer.state.ip)
        ~relative_base:!(computer.state.relative_base)
    with
    | Exit _ -> failwith ("Exited: " ^ Int.to_string addr)
    | Suspended { outputs; ip; relative_base } ->
        context_switch computer.state ip relative_base;
        List.chunks_of ~length:3 outputs
        |> List.map ~f:to_tuple3_exn
        |> List.iter ~f:(fun (target, x, y) ->
               if target = 255 then n.nat := Some (x, y)
               else send_outputs n.node.(target).q x y)

  let run (n : t) =
    while true do
      n.idle := true;
      for addr = 0 to n.length - 1 do
        run_one n addr
      done;
      if !(n.idle) then resume n
    done
end

let () =
  let n =
    In_channel.read_all "./input.txt"
    |> String.strip |> IntCode.read ~len:None |> Network.init ~len:50
  in
  Network.boot n;
  Network.run n
