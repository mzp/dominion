type 'a ch = 'a Ccell.Event.channel

type response = [
| `Ok
| `Rooms of string list
| `Chat  of string
]

type room_req = [
| `Connect  of string
| `Chat     of string * string
]

type request = [
| `ListRoom
| `MakeRoom of string
| room_req
]

module type S = sig
  val connect : string -> int -> (request ch * response ch)
  val server  : string -> int -> f:(request ch -> response ch -> unit) -> unit
end
