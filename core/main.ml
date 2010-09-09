module S = Server.Make(Socket)
module C = Client.Make(Socket)

let port = 8001

let _ =
  if 1 < (Array.length Sys.argv) then
    match Sys.argv.(1) with
	"-client" ->
	  C.connect "127.0.0.1" port
      | _ ->
	  failwith "unknown option"
  else
    S.run "127.0.0.1" port

