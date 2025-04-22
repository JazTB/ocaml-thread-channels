open Thread_Stuff

let f x c_to c_from =
  let rec loop = fun () ->
    let value = EasyChannel.recv c_from in
    Printf.printf "%d -> %d" x value;
    print_newline ();
    EasyChannel.send c_to (value + 1);
    loop ()
  in
  loop

let () =
  let from_main = EasyChannel.create () in
  let to_main = EasyChannel.create () in
  let lst = ref [] in
  for i = 0 to 4 do
    let thr = Domain.spawn (f i to_main from_main) in
    lst := !lst @ [thr];
  done;
  EasyChannel.send from_main 1;
  let rec loop = fun () ->
    let value = EasyChannel.recv to_main in
    match value > 1_000_000 with
    | true -> ()
    | false ->
        EasyChannel.send from_main value;
        loop ()
  in
  loop ()
