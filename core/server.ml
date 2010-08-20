open Base
open Ccell
open Protocol
open ThreadUtils

let ret x f =
  f ();
  x

module Make(T : Protocol.S) = struct
  type game = {
    players : (T.t * (response ch * string)) list;
    dummy : int
  }

  type state = {
    games : (string * (game_req * response ch * T.t) ch) list;
    x : int
  }

  let empty = {
    games = [];
    x = 0;
  }

  let make_game name =
    let master =
      Event.new_channel () in
    let _ =
      state_daemon {players=[]; dummy=42} ~f:begin fun ({players} as s) ->
	let (req, client, id) =
	  Event.sync @@ Event.receive master in
	  match req with
	    | `Join player ->
		Logger.debug "[Game:%s]%s join" name player ();
		Event.sync @@ Event.send client `Ok;
		{ s with players = (id, (client, player)) :: players }
	    | `Say msg ->
		begin match lookup id players with
		    Some (ch, player) ->
		      ret s begin fun () ->
			Logger.debug "[Game:%s]%s say %s" name player msg ();
			ListLabels.iter players ~f:begin fun (_,(ch,_)) ->
			  ignore @@
			    Thread.create (Event.sync $ Event.send ch)
			    (`Chat (player, msg))
			end
		      end
		  | None ->
		      s
		end
	    | _ ->
		s
      end in
      master

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
