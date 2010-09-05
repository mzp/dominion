open Base
open Cc


(*
  'a : client
  'b : request
  'c : result
*)
type ('a,'b,'c) cc = [
| `Cc  of 'c * ('a,'b,'c) action
| `End of 'c
]
and ('a,'b,'c) action = 'a * ('b -> bool) * ('b -> (unit, ('a,'b,'c) cc) Cc.CONT.mc)

type ('a,'b,'c) t = ('a,'b,'c) action option ref
type ('a,'b,'c) suspend = 'a -> ('b -> bool) -> 'c -> (unit, 'b) Cc.CONT.mc

let make () =
  ref None

let end_ x =
  return @@ `End x

let save_cc t cont =
  match Cc.run cont with
      `End x ->
	t := None;
	x
    | `Cc (x, action) ->
	t := Some action;
	x

let suspend prompt : ('a,'b,'c) suspend =
  fun client pred state ->
    let handle k request =
      k @@ return request in
      shiftP prompt (fun k -> return @@ `Cc(state, (client, pred , handle k)))

let start (t : ('a,'b,'c)t) ~f =
  if !t = None then
    Left (save_cc t @@ perform begin
	    p <-- new_prompt ();
	    pushP p @@ f (suspend p)
	  end)
  else
    Right "already start"

let resume t client request =
  match !t with
      Some (client', p, k) ->
	if client' = client then
	  if p request then
	    Left (save_cc t @@ k request)
	  else
	    Right "invalid request"
	else
	  Right "invalid client"
    | None ->
	Right "not start"
