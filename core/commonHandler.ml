open Base
open HandlerBase

module Make(S : Protocol.Rpc) = struct
  module B = HandlerBase.Make(S)
  open B
  type request = [
  | `Join of string
  | `Query of [`Supply | `Mine ]
  | `Say of string
  ]
  type state = S.t HandlerBase.state

  let handle client request ({ clients; game; _ } as state) =
    match request with
      | `Join name ->
	  S.send client `Ok;
	  Left { state with
	      clients = (client, name) :: clients }
      | `Say msg ->
	  ignore @@ Maybe.(perform begin
			     name <-- lookup client clients;
			     return @@ send_all state @@ `Chat (name, msg)
			   end);
	  Left state
      | `Query `Supply ->
	  S.send client @@ `Cards Game.(game.board.supply);
	  Left state
      | `Query `Mine ->
	  let open Game in
	    (S.send client @@
	       match player_of_client client state with
		   Some { hands; _ } ->
		     `Cards hands
		 | None ->
		     `Error "not join");
	    Left state
end
