open Base
open Cc

module type S = sig
  type client
  type request
  type state
end

module Make(S : S) = struct
  type cc = [
  | `Cc  of S.state * action
  | `End of S.state
  ]
  and action = (S.request -> bool) * (S.request -> S.state -> (unit, cc) Cc.CONT.mc)

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

  let run f client state =
    if Hashtbl.length table = 0 then
      ignore @@ save_cc client @@ perform begin
	p <-- new_prompt ();
	pushP p @@ f state
      end

 let handle client request state  =
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
