type 'a ch = 'a Ccell.Event.channel

type player_name = string
type response = [
| `Ok
| `Error of string
| `Games of string list
| `Chat  of player_name * string
| `GameStart
| `Cards of string list
]

type card_id = string
type player_req = [
| `Join of player_name
| `Put of card_id
| `Buy of card_id
| `Part
| `Say of string
| `Ready
]

type game_req = [
| `Create
| `Query of [`Supply | `Mine ]
| `Delete
| player_req
]

type game_name = string
type master = [
  `List
| `Game of game_name * game_req
]

type request = [
  master
]

type 'a peer = {
  id  : 'a;
  req : request ch;
  res : response ch;
}

module type S = sig
  type t
  val connect : string -> int -> t peer
  val server  : string -> int -> f:(t peer -> unit) -> unit
end
