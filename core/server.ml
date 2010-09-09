open Base
open Ccell
open Protocol
open ThreadUtils

module Make(T : Protocol.S) = struct
  let send ch e =
    Event.sync @@ Event.send ch e

  let master ch games =
    let (ch', req) =
      Event.sync @@ Event.receive ch in
      match req with
	| `List id ->
	    send ch' @@ `Games(id, List.map fst games);
	    games
	| `Make (id,name) ->
	    send ch' @@ `Ok id;
	    (name, Handler.create name) :: games
	| `Game(name, request) ->
	    let open Maybe in
	      ignore (perform begin
		 t <-- lookup name games;
		 return @@ Event.sync @@ Handler.handle t ch' request
	       end);
	      games

  let run host port =
    let ch =
      Event.new_channel () in
    let _ =
      state_daemon ~f:(master ch) [] in
      T.server host port ~f:begin fun {req; res; _} ->
	let request =
	  Event.sync @@ Event.receive req in
	  Event.sync @@ Event.send ch (res,request)
      end
end
