open Base
open Cc

(** 'aが結果の型。
    'bが待ち受ける型。 *)
type ('a,'b) cc = [
| `Cc  of 'a * ('a,'b) action
| `End of 'a
]
and ('a,'b) action = 'b -> (unit, ('a,'b) cc) Cc.CONT.mc

type ('a,'b) t = {
  mutable action : ('a,'b) action option;
  mutable value  : 'a option
}
type ('a,'b) suspend = 'a -> (unit, 'b) Cc.CONT.mc

let end_ x =
  return @@ `End x

let save_cc t cont =
  match Cc.run cont with
      `End x ->
	t.value  <- Some x;
	t.action <- None
    | `Cc (x, action) ->
	t.value  <- Some x;
	t.action <- Some action

let suspend p result =
  let handle k x =
    k @@ return x in
    shiftP p (fun k -> return @@ `Cc(result, handle k))

let create f : ('a,'b) t =
  let t = {
    action = None;
    value  = None
  } in
    (save_cc t @@ perform begin
       p <-- new_prompt ();
       pushP p @@ f @@ suspend p
     end);
    t

let resume t x =
  save_cc t @@ Option.get t.action x

let value t =
  Option.get t.value

let is_alive t =
  t.action <> None
