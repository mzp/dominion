open Ccell

type ('a,'b) request = {
  pid     : 'a;
  return   : Protocol.return Ivar.t;
  notify   : Protocol.notify Mbox.t;
  request  : 'b;
}

type 'a t = ('a, Protocol.request) request Event.channel
val start : unit -> 'a t
