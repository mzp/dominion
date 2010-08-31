open Base
open Cc

module type S = sig
  type client
  type request
  type state
end

module Make(S : S) = struct
  type t = [
  | `Cc  of S.state * action
  | `End of S.state
  ]
  and action = (S.request -> bool) * (S.request -> S.state -> (unit, t) Cc.CONT.mc)

  let table : (S.client, action) Hashtbl.t =
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
