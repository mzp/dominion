open Base

type 'a t = int
type 'a result = (unit, (('a * Game.t),string) Base.either) Cc.CONT.mc

let bind   _ = assert false
let many   _ = assert false
let (<|>)  _ = assert false
let lift   _ = assert false
let run _ = assert false
let error _ = assert false
let return _ = assert false
