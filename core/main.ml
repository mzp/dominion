module S = Server.Make(Socket)

let port = 8005

let _ =
  if 1 < (Array.length Sys.argv) && Sys.argv.(1) = "-client" then
    S.connect "127.0.0.1" port
  else
    S.run "127.0.0.1" port

