module S = Server.Make(Socket)
module C = Client.Make(Socket)

let port = 8010

let _ =
  if 1 < (Array.length Sys.argv) && Sys.argv.(1) = "-client" then
    C.connect "127.0.0.1" port
  else
    S.run "127.0.0.1" port

