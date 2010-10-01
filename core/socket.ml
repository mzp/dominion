open Base
open Unix
open Ccell
open Protocol

module M : Protocol.S = struct
  type t = file_descr

  let string_of_bytes n =
    let s =
      String.make 4 ' ' in
      s.[0] <- Char.chr (n land 0xFF);
      s.[1] <- Char.chr ((n lsr 8) land 0xFF);
      s.[2] <- Char.chr ((n lsr 16) land 0xFF);
      s.[3] <- Char.chr ((n lsr 24) land 0xFF);
      s

  let bytes_of_string s =
    (Char.code s.[0]) lor
      (Char.code s.[1] lsl 8) lor
      (Char.code s.[2] lsl 16) lor
      (Char.code s.[3] lsl 24)

  let send sock e =
    let str =
      Marshal.to_string e [] in
    let size =
      String.length str in
      ignore @@ Unix.send sock (string_of_bytes size) 0 4 [];
      ignore @@ Unix.send sock str 0 (String.length str) []

  let recv_until sock size =
      let buf =
	String.make size ' ' in
      let n =
	ref 0 in
	while !n < size do
	  n := !n + (Unix.recv sock buf !n (size - !n) [])
	done;
	buf

  let recv sock =
    let size =
      bytes_of_string (recv_until sock 4) in
      Marshal.from_string (recv_until sock size) 0


  let proxy read write sock =
    ignore @@ Thread.create begin fun () ->
      while true do
	match select [sock] [] [] 0. with
	  | [x],_,_ ->
	      ignore @@	Event.sync @@ Event.send read @@ recv x
	  | _ ->  begin
	      match Event.poll (Event.receive write) with
		  Some e ->
		    send sock e
		| None ->
		    ()
	    end
      done
    end ()

  let socket_with host port f =
    let s =
      Unix.socket PF_INET SOCK_STREAM 0 in
    let _ =
      at_exit (fun () -> shutdown s SHUTDOWN_ALL; close s) in
    let { ai_addr; _ } =
      List.hd @@ getaddrinfo host (string_of_int port) [] in
      f s ai_addr

  let connect host port =
    let read =
      Event.new_channel () in
    let write =
      Event.new_channel () in
      socket_with host port begin fun s addr ->
	connect s addr;
	proxy read write s;
	{
	  id = s;
	  req = write;
	  res = read
	}
      end

  let server host port ~f =
    socket_with host port begin fun s addr ->
      bind   s addr;
      listen s 1;
      while true do
	let (client, _) =
	  accept s in
	let read =
	  Event.new_channel () in
	let write =
	  Event.new_channel () in
	  proxy read write client;
	  ignore @@ Thread.create (fun () ->f { req = read;
						res = write;
						id  = client }) ()
      done
    end
end

include M
