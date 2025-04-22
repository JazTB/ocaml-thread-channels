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

let recv_timeout self timeout_seconds =
  let start_time = Unix.gettimeofday () in
  ignore start_time;
  let rec loop = fun () ->
    match MutexBox.try_lock self with
    | MutexBox.Locked ->
      let now_time = Unix.gettimeofday () in
      let elapsed = now_time -. start_time in
      begin
        match elapsed > timeout_seconds with
        | true -> None
        | false -> loop ()
      end
    | MutexBox.Unlocked value ->
      match !value with
      | [] ->
        MutexBox.unlock self;
        let now_time = Unix.gettimeofday () in
        let elapsed = now_time -. start_time in
        begin
          match elapsed > timeout_seconds with
          | true -> None
          | false -> loop ()
        end
      | [v] ->
        value := [];
        MutexBox.unlock self;
        Some v
      | v :: rest ->
        value := rest;
        MutexBox.unlock self;
        Some v
  in
  loop ()
