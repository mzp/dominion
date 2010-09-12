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
| `Message of game_name * [ `Player of player_name * string
			  | `System of string]
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

