module S = Server.Make(Socket)
module C = Shell.Make(Socket)

let port = 8001

let _ =
  if 1 < (Array.length Sys.argv) then
    match Sys.argv.(1) with
	"-shell" ->
	  let module M =
	    Shell.Make(Socket) in
	    M.connect "127.0.0.1" port
      | "-client" ->
	  let module M =
	    Client.Make(Socket) in
	    M.connect "127.0.0.1" port
      | _ ->
	  failwith "unknown option"
  else
    S.run "127.0.0.1" port
