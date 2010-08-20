open Base
open Ccell
open Protocol
open ThreadUtils

module Make(T : Protocol.S) = struct
  type state = {
    games : (string * (game_req * response ch * T.t) ch) list;
  }
  let empty = {
    games = []
  }


  module R = Rules.Make(struct
			  type t = (T.t * response ch)
			  let equal (x,_) (y,_)= x = y
			  let send (_,ch) e =
			    ignore @@
			      Thread.create (Event.sync $ Event.send ch) e
			end)

  let make_game name =
    let ch =
      Event.new_channel () in
      ret ch begin
	state_daemon (R.empty name) ~f:begin fun state ->
	  let (req, client, id) =
	    Event.sync @@ Event.receive ch in
	    R.run state (id, client) req
	end
      end

  let master ch state =
    let (req, client, id) =
      Event.sync @@ Event.receive ch in
      Logger.debug "accept request" ();
    match req with
      | `List ->
	  Event.sync @@ Event.send client (`Games (List.map fst state.games));
	  state
      | `Game (name,`Create) ->
	  { state with games = (name, make_game name) :: state.games }
      | `Game (name, req) ->
	  begin match lookup name state.games with
	    | Some game ->
		Event.sync @@ Event.send game (req, client,id)
	    | None ->
		Logger.error "no room: %s" name ()
	  end;
	  state

  let run host port =
    let ch =
      Event.new_channel () in
    let _ =
      state_daemon ~f:(master ch) empty in
      T.server host port ~f:begin fun {req; res; id} ->
	let request =
	  Event.sync @@ Event.receive req in
	  Event.sync @@ Event.send ch (request, res, id)
      end
end
