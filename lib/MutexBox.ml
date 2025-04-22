type 'a t = Mutex.t * 'a ref
type 'a lockable = Locked | Unlocked of 'a

let create v =
  (Mutex.create (), ref v)

let lock self =
  let (mutex, value) = self in
  Mutex.lock mutex;
  value

let try_lock self =
  let (mutex, value) = self in
  match Mutex.try_lock mutex with
  | true -> Unlocked value
  | false -> Locked

let unlock self =
  let (mutex, _) = self in
  Mutex.unlock mutex
