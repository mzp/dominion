open Base

type 'a var =
    Bind of int
  | Ref  of int
  | Const of 'a

type source =
    Hands
  | Decks
  | Discards

type action =
  | Draw of int var * source
  | Put  of action * source
  | And  of action * action

type card =
  | Card of int (* and more ... *)

let resolve bs : 'a var -> 'a  option = function
    Bind _ ->
      None
  | Ref b ->
      option (List.assoc b) bs
  | Const x ->
      Some x

type player = {
  hands : card list;
  decks : card list;
  discards : card list
}

let cellar =
  let x = 0
  in
    And (Put((Draw (Bind x,Hands)),Discards),
        (Put((Draw((Ref x),Decks)),Hands)))

type bindisgs =
    (int * int) list
type 'a cont =
    bindisgs -> card list -> player -> 'a

type user_action =
    SelectFromHands of int var * player * user_action cont
  | DrawFrom of source * int var * player * user_action cont
  | Result of player

let map ~f src player =
  match src with
      Decks ->
	f player.decks
    | Hands ->
	f player.hands
    | Discards ->
	f player.discards

let update ~f src player =
    match src with
      Decks ->
	{ player with decks = f player.decks }
    | Hands ->
	{ player with hands = f player.hands }
    | Discards ->
	{ player with discards = f player.discards }


let rec eval bs action player k =
  match action with
    | Draw (x, Hands) ->
	SelectFromHands (x, player, k)
    | Draw (x,src) ->
	begin match resolve bs x with
	    Some n ->
	      k bs
		(map ~f:(HList.take n) src player)
		(update ~f:(HList.drop n) src player)
	  | None ->
	      DrawFrom (src, x, player, k)
	end
    | Put (action, src) ->
	eval bs action player begin fun bs cs p' ->
	  k bs cs (update src p' ~f:(fun x -> cs @ x))
	end
    | And (xs, ys) ->
	eval bs xs player begin fun bs' _ p' ->
	  eval bs' ys p' begin fun bs'' _ p'' ->
	    k bs'' [] p''
	  end
	end

let SelectFromHands (bs,p,k) = eval [] cellar {
  decks=[Card 0; Card 1; Card 2; Card 3];
  hands=[Card 4; Card 5; Card 6];
  discards=[]} (fun bs _ p -> Result p);;

let _ = k [(0,2)] [Card 4; Card 6] {p with hands = [Card 5]};;
