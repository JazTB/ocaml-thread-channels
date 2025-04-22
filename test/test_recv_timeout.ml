open Tc

let () =
  let chan = EasyChannel.create () in
  EasyChannel.send chan 0;
  EasyChannel.send chan 1;
  EasyChannel.send chan 2;
  let rec loop = fun channel ->
    match EasyChannel.recv_timeout channel 1.0 with
    | None ->
      print_endline "Timed out"
    | Some v ->
      Printf.printf "Received %d" v;
      print_newline ();
      loop channel
  in
  loop chan

