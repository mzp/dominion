type level =
    Debug
  | Error
  | Never

val set_level : level -> unit

val debug : ('a, unit, string, unit -> unit) format4 -> 'a
val error : ('a, unit, string, unit -> unit) format4 -> 'a
