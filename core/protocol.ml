type 'a ch = 'a Ccell.Event.channel

type player_name = string
type game_name   = string
type id = string

type game_request = [
  `Query   of id * [
    `Join of player_name
  | `Ready
  | `Select of Game.card
  | `Skip
  | `List of [ `Mine | `Supply ]
  ]
| `Message of string
]
type request = [
  `Game of game_name * game_request
| `List of id
| `Make of id * game_name
]
type response = [
  `Ok      of id
| `Error   of id * string
| `Cards   of id * Game.card list
| `Games   of id * string list
| `Message of game_name * player_name * string
]
(*
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
| `Game  of Game.t
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
]*)

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

module type Rpc = sig
  type t
  val equal : t -> t -> bool
  val send  : t -> response -> unit
end
