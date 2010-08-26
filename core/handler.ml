open Base

module Make(S : Protocol.Rpc) = struct
  module Base   = struct
    type t = S.t
    type state = {
      clients : (t * string) list;
      ready   : t list;
      playing : bool;
      game : Game.t
    }

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

  module Common = CommonHandler.Make(S)(Base)
  module Ready  = ReadyHandler.Make(S)(Base)
  module Player = PlayerHandler.Make(S)(Base)

  open Base
  open ListUtil
  type t = Base.state

  let initial = {
    clients = [];
    ready   = [];
    playing = false;
    game    = Game.make [] []
  }

  let handle client (req : Protocol.game_req) state =
    let state' =
      match req with
	  #Common.request as r ->
	    Common.handle client r state
	| #Ready.request as r ->
	    if state.playing then
	      (S.send client @@ `Error "already started";
	       state)
	    else
	      Ready.handle client r state
	| #Player.request as r ->
	    if client = current_client state then
	      Player.handle client r state
	    else begin
	      S.send client @@ `Error "not your turn";
	      state
	    end
	| `Create ->
	    failwith "must not happen" in
      if state'.playing then
	Player.invoke state'
      else
	state'

  let game { game; _ } =
    game

  let make_dummy xs g =
    { game=g;
      clients=[(List.hd xs, "alice")];
      ready=xs;
      playing =true }
end

