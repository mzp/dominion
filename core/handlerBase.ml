open Base

type 'a state = {
  clients : ('a * string) list;
  ready   : 'a list;
  playing : bool;
  game : Game.t
}

module type S = sig
  type t
  val send_all : t state -> Protocol.response -> unit
  val player_of_client : t -> t state  -> Game.player option
  val current_client   : t state -> t
end

module Make(S : Protocol.Rpc) = struct
  type t = S.t

  let player_of_client client s =
    Maybe.(perform begin
	     name <-- lookup client s.clients;
	     Game.(ListUtil.find (fun p -> p.name = name) s.game.players)
	   end)

  let current_client s =
    let open Game in
      fst @@ List.nth s.clients s.game.me

  let send_all { clients; _} x =
    List.map fst clients
    +> List.iter (flip S.send x)
end
