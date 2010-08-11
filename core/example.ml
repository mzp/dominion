#load "base.cmo";;
#load "hList.cmo";;
#load "cards.cmo";;

open Cards

let cellar =
  let x = 0
  in
    `And (`Put((`Draw (`Bind x,`Hands)),`Discards),
         (`Put((`Draw((`Ref x),`Supply)),`Discards)))

let chapel =
  `Put (`Draw (`Min (`Const 4,`Bind 0),`Hands),`Trash)

let workshop =
  `Put (`Draw (`Const 1,`Filter (`Cost 4,`Supply)), `Discards)

let card id n =
  { id = id;
    cost = n }

let SelectFrom (`Hands,_,g,k) = eval [] cellar
  {me = {
    decks=[];
    hands=[card 0 0; card 1 0; card 2 0];
    discards=[];
  };
   supply=[card 3 0; card 4 0; card 5 0; card 6 0];
   others = [];
   trashs = []}
  (fun _ _ p -> Result p);;

let SelectFrom(`Supply,_,g,k') = k [(0,2)] [card 0 0; card 1 0] {g with me = {g.me with hands = [card 2 0]}};;

k' [(0,2)] [card 3 0; card 4 0] {g with supply = [card 5 0; card 6 0]}
