open Base
open ListUtil
open HandlerBase
open Rule

let observer =
  Observer.make ()

module Make(S : Protocol.Rpc) = struct
  module B = HandlerBase.Make(S)
  open B
  type request = [
  | `Select of Game.card
  | `Skip
  ]
  type state = S.t HandlerBase.state

  let me state =
    Game.((current_player state).name)

  let players state =
    Game.(List.map (fun { name; _ } -> name) state.game.players)

  let others state =
    players state -- [ me state ]

  let request state suspend name =
    let open Cc in
    let client =
      fst @@ List.find (fun (_,y)-> y = name) state.clients in
    let rec f game =
      perform begin
	(client',request) <-- suspend game;
	if client != client' then
	  f game
	else
	  let o =
	    match request with
		`Select c ->
		  Some c
	      | `Skip ->
		  None in
	    return @@ Left(o, game)
      end in
      Rule.lift f

  let of_state suspend state =
  object
    method me =
      me state
    method others =
      others state
    method request =
      request state suspend
  end

  let t = ref None

  let handle client request state  =
    match !t with
	Some t ->
	  Fiber.resume t (client,request);
	  Left { state with game = Fiber.value t  }
      | None ->
	  Right "not invoked"

  let invoke state =
    let t' =
      Fiber.create begin fun suspend ->
	let open Cc in
	  perform begin
	    r <-- Rule.run state.game ~f:(Turn.turn @@ of_state suspend state);
	    match r with
		Left ((), game) ->
		  Fiber.end_ game
	      | Right msg ->
		  failwith msg
	  end
      end
    in
      t := Some t'
end
