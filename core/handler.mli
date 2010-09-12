open Ccell

type 'a t
val create : string -> 'a t
val handle : 'a t -> 'a -> Protocol.response Event.channel -> Protocol.game_request -> unit Event.event
