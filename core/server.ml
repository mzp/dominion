open Base
open Ccell

type 'a ch = 'a Event.channel
module type Transport = sig
  val connect : string -> int -> ('a ch * 'b ch)
  val server  : string -> int -> f:('a ch -> 'b ch -> unit) -> unit
end

type response =
  | Rooms of string list

type request =
  | ListRoom
  | MakeRoom of string

let daemon f =
  Thread.create (forever f) ()

type state = {
  rooms : string list
}

module Make(T : Transport) = struct
  let empty = {
    rooms = []
  }

  let rec manager r w state =
    p "manager" ();
    match Event.sync @@ Event.receive r with
      | (ListRoom,ch) ->
	  Event.sync @@ Event.send ch (Rooms state.rooms);
	  manager r w state
      | (MakeRoom s,_) ->
	  manager r w { rooms = s :: state.rooms }


  let run host port =
    let to_manager =
      Event.new_channel () in
    let from_manager =
      Bcast.make () in
    let _ =
      Thread.create (fun () -> manager to_manager from_manager empty) () in
      T.server host port ~f:begin fun r w ->
	p "accept" ();
	ignore @@ Event.select [
	    Event.wrap (Event.receive r)
	      (fun request -> Event.sync @@ Event.send to_manager (request,w));
	    Event.wrap (Bcast.receive from_manager)
	      (Event.sync $ Event.send w);
	  ]
      end

  let connect host port =
    let (r,w) =
      T.connect host port in
    let _ =
      daemon begin fun () ->
	match Event.sync @@ Event.receive r with
	  | Rooms [] ->
	      p "--> no room" ()
	  | Rooms xs ->
	      List.iter (fun x -> p "--> %s" x ()) xs
      end in
    let f _ = begin
      match read_line () with
	  "ls" ->
	    p "send ls" ();
	    Event.sync @@ Event.send w ListRoom
	| "make" ->
	    p "make room" ();
	    Event.sync @@ Event.send w (MakeRoom "Foo")
	| _ ->
	    ()
    end in
      forever f ()
end
