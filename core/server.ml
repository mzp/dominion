open Base
open Ccell
open Protocol
open ThreadUtils

module Make(T : Protocol.S) = struct
  type room = {
    clients : response ch list;
  }

  type state = {
    rooms : (string * (room_req * response ch) ch) list
  }

  let empty = {
    rooms = []
  }

  let make_room name =
    let ch =
      Event.new_channel () in
    let _ =
      state_daemon {clients=[]} ~f:begin fun s ->
	let (req, client) =
	  Event.sync @@ Event.receive ch in
	  match req with
	    | `Connect _ ->
		Logger.debug "<Room:%s> connect" name ();
		{ clients = client :: s.clients }
	    | `Chat (_, msg) ->
		Logger.debug "<Room:%s> chat: %s" name msg ();
		if List.mem client s.clients then
		  List.iter
		    (fun ch ->
		       thread (fun () ->
				 Event.sync @@
				   Event.send ch @@
				   `Chat msg))
		    s.clients;
		s
      end in
      ch

  let room_name = function
    | `Connect name | `Chat (name,_) ->
	name

  let master (ch : (request * response ch) ch) state =
    let (req, client) =
      Event.sync @@ Event.receive ch in
      Logger.debug "accept request" ();
    match req with
      | `ListRoom ->
	  Event.sync @@ Event.send client (`Rooms (List.map fst state.rooms));
	  state
      | `MakeRoom s ->
	  { rooms = (s, make_room s) :: state.rooms }
      | #room_req as req ->
	  begin match assoc (room_name req) state.rooms with
	    | Some room ->
		Event.sync @@ Event.send room (req, client)
	    | None ->
		Logger.error "no room: %s" (room_name req) ()
	  end;
	  state

  let run host port =
    let ch =
      Event.new_channel () in
    let _ =
      state_daemon ~f:(master ch) empty in
      T.server host port ~f:begin fun r w ->
	let req =
	  Event.sync @@ Event.receive r in
	  Event.sync @@ Event.send ch (req,w)
      end
end
