type 'a t

val create:       unit -> 'a t
val send:         'a t -> 'a -> unit
val recv:         'a t -> 'a
val recv_timeout: 'a t -> float -> 'a option
