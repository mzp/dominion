#use "topfind";;
#require "extlib";;
#load "base.cmo";;
#load "hList.cmo";;
#load "rules.cmo";;
#load "cards.cmo";;
#load "compiler.cmo";;

open Base;;
open Cards;;
open Compiler;;

let card name cost = {
  name = name;
  cost = cost;
}

let game = {
  me = {
    decks=[];
    hands=[card "A" 0; card "B" 1; card "C" 0];
    discards=[];
    action=0;
    draw=0;
    buy=0;
    coin=0;
  };
  supply=[card "D" 0; card "E" 4; card "F" 5; card "G" 10];
  others = [];
  trash = []};;

let select x =
  match x with
      `SelectFrom _ as y ->
	y
    | _ ->
	assert false

let `SelectFrom (cs,num,k) =
  select @@ compile (List.hd mine) game (fun _ x -> `Result x);;

let `SelectFrom (cs',num',k') =
  select @@ k [card "B" 1] game;;

let `SelectFrom (cs'',num'',k'') =
  select @@ k' [card "E" 4] game;;

let _ =
  k'' [card "B" 1] game;;
