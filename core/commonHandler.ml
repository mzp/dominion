open Base

module Make(S : Protocol.Rpc)(B : HandlerBase.S with type t = S.t)  = struct
  open B
  type request = [
  | `Join of string
  | `Query of [`Supply | `Mine ]
  | `Say of string
  ]

  let handle client request ({ clients; game; _ } as state) =
    match request with
      | `Join name ->
	  S.send client `Ok;
	  { state with
	      clients = (client, name) :: clients }
      | `Say msg ->
	  ignore @@ Maybe.(perform begin
			     name <-- lookup client clients;
			     return @@ send_all state @@ `Chat (name, msg)
			   end);
	  state
      | `Query `Supply ->
	  S.send client @@ `Cards Game.(game.board.supply);
	  state
      | `Query `Mine ->
	  let open Game in
	    (S.send client @@
	       match player_of_client client state with
		   Some { hands; _ } ->
		     `Cards hands
		 | None ->
		     `Error "not join");
	    state
end
