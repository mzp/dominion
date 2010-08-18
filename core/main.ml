module S = Server.Make(Socket)


let _ =
  if 1 < (Array.length Sys.argv) && Sys.argv.(1) = "-client" then
    S.connect "127.0.0.1" 8081
  else
    S.run "127.0.0.1" 8081

