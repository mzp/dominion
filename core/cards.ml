open Base

type 'a var =
    [ `Bind of int
    | `Ref  of int
    | `Const of 'a
    | `Min of 'a var * 'a var ]

type pred =
    [ `Cost of int ]

type source =
    [ `Hands
    | `Decks
    | `Discards
    | `Trash
    | `Supply
    | `Filter of pred * source ]

type action =
  [ `Draw of int var * source
  | `Put  of action * source
  | `And  of action * action ]

type card =
    {
      id   : int;
      cost : int
    }

let (>>=) x f =
  match x with
      Some y ->
	f y
    | None ->
	None

let rec resolve bs : 'a var -> 'a  option = function
    `Bind _ ->
      None
  | `Ref b ->
      option (List.assoc b) bs
  | `Const x ->
      Some x
  | `Min(x,y) ->
      resolve bs x >>=
	(fun x' ->
	   resolve bs y >>=
	     (fun y' ->
		Some (min x' y')))

type player = {
  hands : card list;
  decks : card list;
  discards : card list;
}

type game = {
  me     : player;
  others : player list;
  trashs : card list;
  supply : card list
}

type bindisgs =
    (int * int) list

type 'a cont =
    bindisgs -> card list -> game -> 'a

type user_action =
    SelectFrom of [ `Hands | `Supply ] * int var * game    * user_action cont
  | DrawFrom   of source  * int var * player * user_action cont
  | Result     of game

let fun_of_pred = function
    `Cost n ->
      fun c -> c.cost <= n

let rec map ~f src ({me; trashs; supply} as game) =
  match src with
      `Decks ->
	f me.decks
    | `Hands ->
	f me.hands
    | `Discards ->
	f me.discards
    | `Trash ->
	f trashs
    | `Supply ->
	f supply
    | `Filter (pred, src) ->
	map src game ~f:(fun cs -> f (List.filter (fun_of_pred pred) cs))

let rec update ~f src ({ me; trashs; supply } as game) =
    match src with
      `Decks ->
	{ game with me = { me with decks = f me.decks } }
    | `Hands ->
	{ game with me = { me with hands = f me.hands } }
    | `Discards ->
	{ game with me = { me with discards = f me.discards } }
    | `Trash ->
	{ game with trashs = f trashs }
    | `Supply ->
	{ game with supply = f supply }
    | `Filter (pred,src) ->
	update src game ~f:begin fun cs ->
	  let (xs, ys) =
	    List.partition (fun_of_pred pred) cs in
	    (f xs)@ys
	end

let rec eval bs action game k =
  match action with
    | `Draw (x, `Hands) ->
	SelectFrom (`Hands, x, game, k)
    | `Draw (x, `Supply) ->
	SelectFrom (`Supply, x, game, k)
    | `Draw (x,src) ->
	begin match resolve bs x with
	    Some n ->
	      k bs
	        (map ~f:(HList.take n) src game)
		(update ~f:(HList.drop n) src game)
	  | None ->
	      DrawFrom (src, x, game.me, k)
	end
    | `Put (action, src) ->
	eval bs action game begin fun bs cs g ->
	  k bs cs (update src g ~f:(fun x -> cs @ x))
	end
    | `And (xs, ys) ->
	eval bs xs game begin fun bs' _ g ->
	  eval bs' ys g begin fun bs'' _ g' ->
	    k bs'' [] g'
	  end
	end
