open Tc

type 'a should_die = Die | Live of 'a

let f x c_to c_from =
  let rec loop = fun () ->
    match EasyChannel.recv c_from with
    | Die -> ()
    | Live value ->
      Printf.printf "%d -> %d" x value;
      print_newline ();
      EasyChannel.send c_to (value + 1);
      loop ()
  in
  loop

let () =
  let from_main = EasyChannel.create () in
  let to_main = EasyChannel.create () in
  let rec spawn = fun cur max threads ->
    match cur = max with
    | true -> threads
    | false ->
      let new_thread = Domain.spawn (f cur to_main from_main) in
      spawn (cur + 1) max (threads @ [new_thread])
  in
  let lst = spawn 0 4 [] in

  EasyChannel.send from_main (Live 1);
  let rec loop = fun () ->
    let value = EasyChannel.recv to_main in
    match value > 1_000 with
    | true ->
        for _ = 0 to 4 do
          EasyChannel.send from_main Die;
        done;
    | false ->
        EasyChannel.send from_main (Live value);
        loop ()
  in
  loop ();

  let rec kill = fun threads ->
    match threads with
    | [] -> ()
    | [v] -> Domain.join v;
    | v :: rest ->
        Domain.join v;
        kill rest;
  in
  kill lst
