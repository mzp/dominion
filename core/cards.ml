open Rules
open Cc
let some_card p =
  perform begin
    x <-- shiftP p (fun k -> return (`Cont k));
    return (`Other (x+1))
  end

let _ =
  run (perform begin
	 p <-- new_prompt ();
	 r <-- pushP p (some_card p);
	 match r with
	   | `Cont k ->
	       k (return 1)
	   |_ ->
	       assert false
       end)
