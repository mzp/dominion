open Base
open Curses
open Ccell
open ThreadUtils
open Protocol

module Make(T : Protocol.S) = struct
  let strip s =
    let n =
      String.index s '\000' in
      String.sub s 0 n

  let game =
    ref ""

  let send req line =
    match Str.split (Str.regexp " ") @@ line with
	["/rooms"] ->
	  Event.sync @@ Event.send req `List
      | ["/room"; name] ->
	  Event.sync @@ Event.send req (`Game (name,`Create))
      | ["/join"; x; y] ->
	  game := x;
	  Event.sync @@ Event.send req (`Game (x,`Join y))
      | "/chat"::msgs ->
	  Event.sync @@ Event.send req (`Game (!game,
					       `Say (String.concat " " msgs)))
      | ["/ready"] ->
	  Event.sync @@ Event.send req (`Game (!game,`Ready))
      | ["/query"; "supply"] ->
	  Event.sync @@ Event.send req (`Game (!game,`Query `Supply))
      | ["/query"; "mine"] ->
	  Event.sync @@ Event.send req (`Game (!game,`Query `Mine))
      | ["/skip"] ->
	  Event.sync @@ Event.send req (`Game (!game,`Skip))
      | ["/select"; c] ->
	  Event.sync @@ Event.send req (`Game (!game,`Select (Game.of_string c)))
      | ["/q"] ->
	  endwin ();
	  exit 0
      | _ ->
	  ()

  let to_string = function
    | `Games xs ->
	Left (Std.dump xs)
    | `Chat (name,msg) ->
	Left (Printf.sprintf "[%s]%s" name msg)
    | `Ok ->
	Left ("ok")
    | `Error s ->
	Left (Printf.sprintf "error: %s" s)
    | `GameStart ->
	Left "game start"
    | `Cards xs ->
	Left (Std.dump @@ List.map (fun x -> p "%s" (Game.to_string x) ()) xs)
    | `Turn name ->
	Left (Printf.sprintf "turn: %s" name)
    | `Phase (`Action, name) ->
	Left (Printf.sprintf "action phase %s" name)
    | `Phase (`Buy, name) ->
	Left (Printf.sprintf "buy phase %s" name)
    | `Phase (`Cleanup, name) ->
	Left (Printf.sprintf  "clienup phase %s" name)
    | `Notify s ->
	Left (Printf.sprintf  "notify %s" s)
    | `Game g ->
	Right (Game.show g)
    | _ ->
	Left "unkwnown"

  let wait_loop (game, response) ch xs =
    let e =
      Event.receive ch in
      match Event.poll e with
	  Some x ->
	    begin match to_string x with
		Left x ->
		  ignore @@ wclear   response;
		  ignore @@ mvwaddstr response 0 0 @@ String.concat "\n" (x::xs);
		  ignore @@ wrefresh response;
		  ignore @@ Curses.refresh ();
		  x::xs
	      | Right g ->
		  ignore @@ wclear game;
		  ignore @@ mvwaddstr game 0 0 g;
		  ignore @@ wrefresh game;
		  xs
	    end
	| None ->
	    xs

  let prompt_loop prompt req () =
    let s =
      String.make 30 ' ' in
      match Unix.select [ Unix.stdin ] [] [] 0.0 with
	  [],_,_ ->
	    ()
	| _ ->
	    ignore @@ wgetstr prompt s;
	    ignore @@ wclear prompt;
	    ignore @@ wrefresh prompt;
	    send req @@ strip s

  let connect host port =
    let {res; req; _ } =
      T.connect host port in
    let top =
      Curses.initscr () in
    let game =
      Curses.subwin top 20 80 1 0 in
    let response =
      Curses.subwin top 10 20 1 80 in
    let prompt =
      Curses.subwin top 1 80 0 2 in
    let _ =
      ignore @@ mvaddch 0 0 (int_of_char '$');
      ignore @@ Curses.mvwaddstr game 0 0 (Game.show @@ Game.make [] [`Cellar]);
      ignore @@ Curses.mvwaddstr response 0 0 "";
      ignore @@ Curses.mvwaddstr prompt 0 0 "";
      ignore @@ immedok top true in
    let rec iter a b =
      let _ =
	Curses.refresh () in
	iter (wait_loop (game,response) res a) (prompt_loop prompt req b) in
      iter [] ()
(*    let _ =
      wait_loop (game,response) res in
      while true do
	let _ =
	  Curses.refresh () in
      done*)
end
