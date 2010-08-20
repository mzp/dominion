let debug fmt =
  Printf.kprintf (fun s () ->
		    prerr_string "[DEBUG]";
		    prerr_endline s;
		    flush stderr)
    fmt


let error fmt =
  Printf.kprintf (fun s () ->
		    prerr_string "[ERROR]";
		    prerr_endline s;
		    flush stderr)
    fmt
