open Base
open Ccell

type 'a ch = 'a Event.channel

type response = [
| `Rooms of string list
]

type request = [
| `ListRoom
| `MakeRoom of string
]

module type Transport = sig
  val connect : string -> int -> (request ch * response ch)
  val server  : string -> int -> f:(request ch -> response ch -> unit) -> unit
end

let daemon f =
  Thread.create (forever f) ()

type state = {
  rooms : string list
}

let state_daemon f init =
  let rec g state =
    g (f state) in
    Thread.create g init

module Make(T : Transport) = struct
  let empty = {
    rooms = []
  }

  let master (ch : (request * response ch) ch) state =
    p "master" ();
    match Event.sync @@ Event.receive ch with
      | `ListRoom, client ->
	  Event.sync @@ Event.send client (`Rooms state.rooms);
	  state
      | `MakeRoom s, _ ->
	  { rooms = s :: state.rooms }

  let run host port =
    let ch =
      Event.new_channel () in
    let _ =
      state_daemon (master ch) empty in
      T.server host port ~f:begin fun r w ->
	p "accept" ();
	ignore @@ Event.select [
	    Event.wrap (Event.receive r)
	      (fun request -> Event.sync @@ Event.send ch (request,w));
	  ]
      end

  let connect host port =
    let (w,r) =
      T.connect host port in
    let _ =
      daemon begin fun () ->
	match Event.sync @@ Event.receive r with
	  | `Rooms [] ->
	      p "--> no room" ()
	  | `Rooms xs ->
	      List.iter (fun x -> p "--> %s" x ()) xs
      end in
    let f _ = begin
      match read_line () with
	  "ls" ->
	    p "send ls" ();
	    Event.sync @@ Event.send w `ListRoom
	| "make" ->
	    p "make room" ();
	    Event.sync @@ Event.send w (`MakeRoom "Foo")
	| _ ->
	    ()
    end in
      forever f ()
end
