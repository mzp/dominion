module type S = sig
  type t
  type state = {
    clients : (t * string) list;
    ready   : t list;
    playing : bool;
    game : Game.t
  }

  val send_all : state -> Protocol.response -> unit
  val player_of_client : t -> state  -> Game.player option
  val current_client   : state -> t
end
