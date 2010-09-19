open Base
open Ccell
open Protocol
open ThreadUtils

module Make(T : Protocol.S) = struct
  let bind x f = Event.wrap x (fun v -> Event.sync (f v))

  let on_request {id = pid ; req; res } system notify =
    let return =
      Ivar.make () in
      perform begin
	request <-- Event.receive req;
	Event.send system { GameThread.pid; request; notify; return };
	value <-- Ivar.read return;
	Event.send res (value :> Protocol.response)
      end

  let rec on_notify peer notify =
    perform begin
      value <-- Mbox.pop notify;
      Event.send peer.res (value :> Protocol.response);
      on_notify peer notify
    end

  let run host port =
    let system =
      GameThread.start () in
      T.server host port ~f:begin fun client ->
	let mbox =
	  Mbox.make () in
	let _ =
	  Thread.create (fun () -> Event.sync @@ on_notify client mbox) () in
	  forever (fun () -> Event.sync @@ on_request client system mbox) ()
      end
end
