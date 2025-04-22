type 'a t
type 'a lockable = Locked | Unlocked of 'a

val create:   'a   -> 'a t
val lock:     'a t -> 'a ref
val try_lock: 'a t -> 'a ref lockable
val unlock:   'a t -> unit
