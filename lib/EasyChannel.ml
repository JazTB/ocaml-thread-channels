type 'a t = 'a list MutexBox.t

let create () =
  MutexBox.create ([]: 'a list)

let send self new_v =
  let v = MutexBox.lock self in
  v := !v @ [new_v];
  MutexBox.unlock self

let recv self =
  let rec loop = fun () ->
    let potential_old_value = MutexBox.try_lock self in
    match potential_old_value with
    | MutexBox.Locked -> loop ()
    | MutexBox.Unlocked value ->
      match !value with
      | [] ->
        MutexBox.unlock self;
        loop ()
      | [v] ->
        value := [];
        MutexBox.unlock self;
        v
      | v :: rest ->
        value := rest;
        MutexBox.unlock self;
        v
  in
  loop ()
