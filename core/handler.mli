type t
val create : string -> t
val handle : t -> Protocol.response Event.channel -> Protocol.game_request -> unit Event.event
