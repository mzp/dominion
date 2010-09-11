open Base
type level =
    Debug
  | Error
  | Never

let to_int level =
  fst @@ ExtList.List.findi (fun _ x -> level = x) [ Never; Error; Debug]

let level = ref Debug
let set_level l =
  level := l

let (<%) x y =
  to_int x <= to_int y

let print l prefix fmt =
  Printf.kprintf (fun s () ->
		    if l <% !level then begin
		      prerr_string prefix;
		      prerr_endline s;
		      flush stderr
		    end)
    fmt

let debug fmt =
  print Debug "[DEBUG]" fmt

let error fmt =
  print Error "[ERROR]" fmt
