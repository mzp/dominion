open Base

let daemon f =
  Thread.create (forever f) ()

let state_daemon ~f init =
  let rec g state =
    g (f state) in
    Thread.create g init

let thread f =
  ignore @@ Thread.create f ()
