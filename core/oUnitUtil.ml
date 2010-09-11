open Xml
open Unix
include OUnit

let date t =
  let { tm_sec; tm_min; tm_hour; tm_mday; tm_mon; tm_year; _ } =
    Unix.localtime t in
  [
    Element ("date",[("format", "YYYYMMDD");
		     ("val", Printf.sprintf "%04d%02d%2d" (tm_year+1900) (tm_mon+1) tm_mday)
		    ],[]);
    Element ("time",[("format","HHMMSS");
		     ("val", Printf.sprintf "%02d%02d%0d2" tm_hour tm_min tm_sec)
		    ],[])
  ]

let test =
  let make path passed descr state =
    Element ("test",[("name", string_of_path path);
		     ("executed","yes")],
	     [Element ("description",[], [ PCData descr]);
	      Element ("result",[],[
			 Element ("success",[("passed",passed);
					     ("state",state)],[])
		       ])]) in
  function
    RSuccess path ->
      make path "yes" "-" "success"
  | RFailure (path, descr) ->
      make path "no" descr "failure"
  | RError (path, descr) ->
      make path "no" descr "error"
  | RSkip (path, descr) ->
      make path "no" descr "skip"
  | RTodo (path, descr) ->
      make path "no" descr "todo"

let rec was_successful results =
  match results with
      [] -> true
    | RSuccess _::t
    | RSkip _::t -> was_successful t
    | RFailure _::_
    | RError _::_
    | RTodo _::_ -> false

let get_name = function
  | TestCase _ ->
      "unknown"
  | TestList _ ->
      "unknown"
  | TestLabel (name,_) ->
      name


let run_test_xml_main suite =
  let verbose = ref false in
  let only_test = ref [] in
    Arg.parse
      (Arg.align
         [("-verbose", Arg.Set verbose, " Run the test in verbose mode.");
          ("-only-test", Arg.String (fun str -> only_test := str :: !only_test),
           "path Run only the selected test");
         ]
      )
      (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
      ("usage: " ^ Sys.argv.(0) ^ " [-verbose] [-only-test path]*");

    let nsuite =
      if !only_test = [] then
        (
          suite
        )
      else
        (
          match test_filter !only_test suite with
            | Some tst ->
                tst
            | None ->
                failwith ("Filtering test "^
                          (String.concat ", " !only_test)^
                          " lead to no test")
        )
    in
    let start  = Unix.time () in
    let result = run_test_tt ~verbose:!verbose nsuite in
    let end_   = Unix.time () in
    let ch =
      open_out (get_name suite ^ ".xml") in
    let xml =
      Element ("report",[],
	       List.concat [
		 [ Element ("start",[], date start) ];
		 List.map test result;
		 [Element ("end",[], date end_) ]
	       ]) in
    let _ =
      output_string ch ((Xml.to_string_fmt xml)^"\n") in
      if not (was_successful result) then
	exit 1
      else
	result

