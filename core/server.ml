open Base
open Ccell
open Protocol
open ThreadUtils

module Make(T : Protocol.S) = struct
  let on_recv {id; req; res } system mbox =
    let request =
      Event.sync @@ Event.receive req in
    let return =
      Ivar.make () in
(*    let return_e =
 in
    let notify_e =
      Event.wrap (Mbox.pop mbox) @@ fun r ->
	Event.sync @@ Event.send res (r :> Protocol.response) in
    let rec loop () =
      match Event.select [ Event.wrap return_e @@ const `Return;
			   Event.wrap notify_e @@ const `Notify ] with
	| `Return -> ()
    in*)
      Event.sync @@ Event.send system {
	GameThread.pid = id;
	request;
	notify = mbox;
	return;
      };
      Event.sync @@ Event.wrap (Ivar.read return) @@ fun r ->
	Event.sync @@ Event.send res (r :> Protocol.response)

  let run host port =
    let system =
      GameThread.start () in
      T.server host port ~f:begin fun client ->
	let notify =
	  Mbox.make () in
	  forever (fun () -> on_recv client system notify) ()
      end
end
