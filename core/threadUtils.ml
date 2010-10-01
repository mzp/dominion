open Base
open Ccell

let daemon f =
  Thread.create (forever f) ()

let state_daemon ~f init =
  let rec g state =
    g (f state) in
    Thread.create g init

let thread f =
  ignore @@ Thread.create f ()

let with_channel f =
  let ch =
    Event.new_channel () in
  let _ =
    f ch in
    ch
