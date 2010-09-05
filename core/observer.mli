type 'a t
type 'a listener = 'a -> unit

val listen : 'a t -> 'a listener -> unit
val clear  : 'a t -> unit
val map    : ('a -> 'b) -> 'a t -> 'b t
val filter : ('a -> 'b option) -> 'a t -> 'b t
val merge  : 'a t -> 'b t -> ('a,'b) Base.either t
val __fire : 'a t -> 'a -> unit
val make   : unit -> 'a t
