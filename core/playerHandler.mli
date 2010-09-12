(** プレイヤーの操作のうち、更新系をハンドリングする *)

(** 扱うリクエスト *)
type request = [
| `Select of Game.card
| `Skip
]

type 'a fiber

(** 必要な情報 *)
class type ['a] t = object('b)
  method fiber     : 'a fiber option
  method set_fiber : 'a fiber option -> 'b
  method observer  : Game.t Observer.t
  method game      : Game.t
  method set_game  : Game.t -> 'b
  method clients   : (string * 'a) list
end

(** ゲーム開始 *)
val invoke : ('a #t as 'b) -> 'b

(** 更新系リクエストのハンドリング *)
val handle : ('a #t as 'b) -> 'a -> request -> ('b,string) Base.either
