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
and ('a,'b,'c) action = ('b -> bool) * ('b -> 'c -> (unit, ('a,'b,'c) cc) Cc.CONT.mc)

type ('a,'b,'c) t = ('a, ('a,'b,'c) action) Hashtbl.t
type ('a,'b,'c) suspend = ('b -> bool) -> 'c -> (unit, 'b * 'c ) Cc.CONT.mc

let make () =
  Hashtbl.create 0

let end_ state =
  return @@ `End state

let save_cc table client cont =
  match Cc.run cont with
      `End state ->
	Hashtbl.clear table;
	state
    | `Cc (state, (pred, cc)) ->
	Hashtbl.add table client (pred, cc);
	state

let cc prompt pred state =
  let handle k request state =
    k @@ return (request, state) in
    shiftP prompt (fun k -> return @@ `Cc(state, (pred , handle k)))

let start table client state ~f =
  if Hashtbl.length table = 0 then
    Left (save_cc table client @@ perform begin
	    p <-- new_prompt ();
	    pushP p @@ f (cc p) state
	  end)
  else
    Right "already start"

let resume (table : ('a,'b,'c) t) client request state =
  if Hashtbl.mem table client then
    let (p, k) =
      Hashtbl.find table client in
      if p request then
	Left (save_cc table client @@ k request state)
      else
	Right "invalid request";
  else
    Right "invalid client"



(*  let table : (S.client, action) Hashtbl.t =
    Hashtbl.create 0

  let save_cc client cont =
    match Cc.run cont with
	`End state ->
	  Hashtbl.clear table;
	  state
      | `Cc (state, (pred, cc)) ->
	  Hashtbl.add table client (pred, cc);
	  state

  let end_ state = return @@ `End state

  type suspend =
      (S.request -> bool) -> S.state -> (unit, S.request * S.state) Cc.CONT.mc

  let cc prompt pred state =
    let handle k request state =
      k @@ return (request, state) in
      shiftP prompt (fun k -> return @@ `Cc(state, (pred , handle k)))

  let start f client state =
    if Hashtbl.length table = 0 then
      Left (save_cc client @@ perform begin
	      p <-- new_prompt ();
	      pushP p @@ f (cc p) state
	    end)
    else
      Right "already start"

 let resume client request state  =
   if Hashtbl.mem table client then
     let (p, k) =
       Hashtbl.find table client in
       if p request then
	 Left (save_cc client @@ k request state)
       else
	 Right "invalid request";
   else
     Right "invalid client"
end
*)
