type 'a ch = 'a Ccell.Event.channel

type player_name = string
type response = [
| `Ok
| `Error of string
| `Games of string list
| `Chat  of player_name * string
]

type card_id = string
type player_req = [
| `Join of player_name
| `Query
| `Put of card_id
| `Buy of card_id
| `Part
| `Say of string
]

type game_req = [
| `Create
| `Query
(* | `Update TODO: 利用カードなどを設定できるようにする *)
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
