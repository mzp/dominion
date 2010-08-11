open Base

type 'a var =
    Bind of int
  | Ref  of int
  | Const of 'a
  | Min of 'a var * 'a var

type source =
    Hands
  | Decks
  | Discards
  | Trash

type action =
  | Draw of int var * source
  | Put  of action * source
  | And  of action * action

type card =
  | Card of int (* and more ... *)

let rec resolve bs : 'a var -> 'a  option = function
    Bind _ ->
      None
  | Ref b ->
      option (List.assoc b) bs
  | Const x ->
      Some x
  | Min(x,y) ->
      min (resolve bs x) (resolve bs y)

type player = {
  hands : card list;
  decks : card list;
  discards : card list;
}

type game = {
  me     : player;
  others : player list;
  trashs : card list
}

let cellar =
  let x = 0
  in
    And (Put((Draw (Bind x,Hands)),Discards),
        (Put((Draw((Ref x),Decks)),Hands)))

let chapel =
  Put (Draw (Min (Const 4,Bind 0),Hands),Trash)

type bindisgs =
    (int * int) list

type 'a cont =
    bindisgs -> card list -> game -> 'a

type user_action =
    SelectFromHands of int var * game    * user_action cont
  | DrawFrom        of source  * int var * player * user_action cont
  | Result          of game

let map ~f src {me; trashs} =
  match src with
      Decks ->
	f me.decks
    | Hands ->
	f me.hands
    | Discards ->
	f me.discards
    | Trash ->
	f trashs

let update ~f src ({ me; trashs } as game) =
    match src with
      Decks ->
	{ game with me = { me with decks = f me.decks } }
    | Hands ->
	{ game with me = { me with hands = f me.hands } }
    | Discards ->
	{ game with me = { me with discards = f me.discards } }
    | Trash ->
	{ game with trashs = f trashs }

let rec eval bs action game k =
  match action with
    | Draw (x, Hands) ->
	SelectFromHands (x, game, k)
    | Draw (x,src) ->
	begin match resolve bs x with
	    Some n ->
	      k bs
	        (map ~f:(HList.take n) src game)
		(update ~f:(HList.drop n) src game)
	  | None ->
	      DrawFrom (src, x, game.me, k)
	end
    | Put (action, src) ->
	eval bs action game begin fun bs cs g ->
	  k bs cs (update src g ~f:(fun x -> cs @ x))
	end
    | And (xs, ys) ->
	eval bs xs game begin fun bs' _ g ->
	  eval bs' ys g begin fun bs'' _ g' ->
	    k bs'' [] g'
	  end
	end

(* example *)

let SelectFromHands (bs,g,k) = eval [] chapel
  {me = {
    decks=[Card 0; Card 1; Card 2; Card 3];
    hands=[Card 4; Card 5; Card 6];
    discards=[]
  };
   others = [];
   trashs = []}
  (fun _ _ p -> Result p);;

let _ = k [(0,2)] [Card 4; Card 6] {g with me = {g.me with hands = [Card 5]}};;
