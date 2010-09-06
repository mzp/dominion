open Base
open HandlerBase
open ListUtil

module Make(S : Protocol.Rpc) = struct
  module B = HandlerBase.Make(S)
  module Common = CommonHandler.Make(S)
  module Ready  = ReadyHandler.Make(S)
  module Player = PlayerHandler.Make(S)

  open B
  type t = Common.state

  let initial = {
    clients = [];
    ready   = [];
    playing = false;
    game    = Game.make [] []
  }

  let handle client (req : Protocol.game_req) state =
    let _ =
      Observer.clear PlayerHandler.observer;
      Observer.listen  PlayerHandler.observer begin fun g ->
	List.iter (fun (client, _) ->
		     S.send client @@ `Game g) state.clients
      end
    in
    let state' =
      match req with
	  #Common.request as r ->
	    Common.handle client r state
	| #Ready.request as r ->
	    if state.playing then
	      Right "already started"
	    else
	      Ready.handle client r state
	| #Player.request as r ->
	    if client = current_client state then
	      Player.handle client r state
	    else
	      Right "not your turn"
	| `Create ->
	    failwith "must not happen" in
      match state' with
	  Left s when s.playing ->
	    Player.invoke s;
	    state'
	| Left _ | Right _ ->
	    state'

  let game { game; _ } =
    game

  let make_dummy xs g =
    { game=g;
      clients=[(List.hd xs, "alice")];
      ready=xs;
      playing =true }
end

