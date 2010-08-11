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
  | Treasure (* and more ... *)


let resolve bs = function
    Bind _ ->
      assert false
  | Ref b ->
      sure (List.assoc b bs)
  | Const x ->
      x

type player = {
  hands : card list;
  decks : card list;
  discards : card list
}

let cellar =
  let x = 0
  in
    And (Put((Draw (Bind x,Hands)),Discards),
         (Draw((Ref x),Decks)))

type bindisgs =
    (int * int) list
type 'a cont =
    bindisgs -> card list -> player -> 'a

type user_action =
    SelectFromHands of int option * user_action cont

let rec eval bs action player k =
  match action with
    | Draw (x, Hands) ->
	SelectFromHands
    | Put  of action * source
    | And  of action * action
