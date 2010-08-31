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
  val current_player   : t state -> Game.player
  val next_turn        : t state -> t state
end

module Make(S : Protocol.Rpc) = struct
  type t = S.t

  let player_of_client client s =
    Maybe.(perform begin
	     name <-- lookup client s.clients;
	     Game.(ListUtil.find (fun p -> p.name = name) s.game.players)
	   end)

  open Game

  let current_player s =
    Game.me s.game

  let current_client s =
    let {name; _ } =
      current_player s in
      fst @@ List.find (fun (_,y)-> y = name) s.clients

  let next_turn s =
    { s with game = { s.game with me = s.game.me + 1 mod (List.length s.ready) }}


  let send_all { clients; _} x =
    List.map fst clients
    +> List.iter (flip S.send x)
end
