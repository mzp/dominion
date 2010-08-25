type 'a ch = 'a Ccell.Event.channel

type player_name = string

type notify = [
| `GameStart
| `Turn of player_name
| `Phase of [`Action | `Buy | `Cleanup] * player_name
| `Notify of string
]

type response = [
| `Ok
| `Error of string
| `Games of string list
| `Chat  of player_name * string
| `Cards of Game.card list
| notify
]

type card_id = string
type player_req = [
| `Join of player_name
| `Select of Game.card
| `Skip
| `Say of string
| `Ready
]

type game_req = [
| `Create
| `Query of [`Supply | `Mine ]
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
