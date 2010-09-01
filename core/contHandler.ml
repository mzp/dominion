open Base
open Cc


(*
  'a : client
  'b : request
  'c : state
*)
type ('a,'b,'c) cc = [
| `Cc  of 'c * ('a,'b,'c) action
| `End of 'c
]
and ('a,'b,'c) action = 'a * ('b -> bool) * ('b -> 'c -> (unit, ('a,'b,'c) cc) Cc.CONT.mc)

type ('a,'b,'c) t = ('a,'b,'c) action option ref
type ('a,'b,'c) suspend = 'a -> ('b -> bool) -> 'c -> (unit, 'b * 'c ) Cc.CONT.mc

let make () =
  ref None

let end_ state =
  return @@ `End state

let save_cc t cont =
  match Cc.run cont with
      `End state ->
	t := None;
	state
    | `Cc (state, action) ->
	t := Some action;
	state

let suspend prompt client pred state =
  let handle k request state =
    k @@ return (request, state) in
    shiftP prompt (fun k -> return @@ `Cc(state, (client, pred , handle k)))

let start t state ~f =
  if !t = None then
    Left (save_cc t @@ perform begin
	    p <-- new_prompt ();
	    pushP p @@ f (suspend p) state
	  end)
  else
    Right "already start"

let resume t client request state =
  match !t with
      Some (client', p, k) ->
	if client' = client then
	  if p request then
	    Left (save_cc t @@ k request state)
	  else
	    Right "invalid request"
	else
	  Right "invalid client"
    | None ->
	Right "not start"
