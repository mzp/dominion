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

  let send ch e =
    Event.sync @@ Event.send ch e

  let send (req : Protocol.request Ccell.Event.channel) line =
    let id =
      string_of_int @@ Random.int 100 in
    match Str.split (Str.regexp " ") @@ line with
	["/ls"] ->
	  send req @@ `List id
      | ["/make"; name] ->
	  send req @@ `Make (id, name)
      | ["/join"; x; y] ->
	  game := x;
	  send req @@ `Game (x,(`Query (id, `Join y)))
      | "/chat"::msgs ->
	  send req @@
	    `Game (!game,
		   `Message (String.concat " " msgs))
      | ["/ready"] ->
	  send req @@
	    `Game (!game,`Query (id,`Ready))
      | ["/query"; "supply"] ->
	  send req @@
	    `Game (!game,
		   `Query (id, `List `Supply))
      | ["/query"; "mine"] ->
	  send req @@
	    `Game (!game,
		   `Query (id, `List `Mine))
      | ["/skip"] ->
	  send req @@
	    `Game (!game,`Query (id,`Skip))
      | ["/select"; c] ->
	  send req @@
	    `Game (!game,`Query (id, `Select (Game.of_string c)))
      | ["/q"] ->
	  endwin ();
	  exit 0
      | _ ->
	  ()

  let to_string = function
    | `Ok _ ->
	Left ("ok")
    | `Error (_,s) ->
	Left (Printf.sprintf "error: %s" s)
    | `Games (_,xs) ->
	Left (Std.dump xs)
    | `Cards (_,xs) ->
	Left (Std.dump @@ List.map (fun x -> p "%s" (Game.to_string x) ()) xs)
    | `Message (game, name,msg) ->
	Left (Printf.sprintf "%s@%s: %s" name game msg)

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

  let make w ~l ~c ~x ~y =
    Curses.subwin w l c y x

  let connect host port =
    let (h,w) =
      Curses.get_size () in
    let {res; req; _ } =
      T.connect host port in
    let top =
      Curses.initscr () in
    let w' =
      (w*2)/3 in
    let game =
      make top ~l:(h-1) ~c:w' ~y:1 ~x:0 in
    let response =
      make top ~l:(h-1) ~c:(w - w') ~y:1 ~x:w' in
    let prompt =
      make top ~l:1 ~c:80 ~y:0 ~x:2 in
    let _ =
      ignore @@ mvaddch 0 0 (int_of_char '$');
      ignore @@ Curses.mvwaddstr game 1 1 (Game.show @@ Game.make [] [`Cellar]);
    let rec iter a b =
      let _ =
	Curses.refresh () in
	iter (wait_loop (game,response) res a) (prompt_loop prompt req b) in
      iter [] ()
end
