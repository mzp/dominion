open Base
open Unix

module M : Server.Transport = struct
  type 'a channel = file_descr
  type 'a event   =
      Send of file_descr * string
    | Recv of file_descr

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

  let send ch v =
    Send (ch, Marshal.to_string v [])

  let receive ch =
    Recv ch

  let choose xs =
    let readers =
      filter_map (function Recv ch -> Some ch | _ -> None) xs in
    let writers =
      filter_map (function Send (ch,_) -> Some ch | _ -> None) xs in
      match select readers writers [] (-1.) with
	  [],[],_ ->
	    assert false
	| (x::_),_,_ ->
	    Recv x
	| [],x::_,_ ->
	    List.find (function
			   Send (fd,_) when fd = x ->
			     true
			 | _ ->
			     false)
	      xs

  let sync = function
      Send (sock, str) ->
	let size =
	  String.length str in
	  ignore @@ Unix.send sock (string_of_bytes size) 0 4 [];
	  ignore @@ Unix.send sock str 0 (String.length str) [];
	  Obj.magic () (* must be unit *)
    | Recv sock ->
	let recv n =
	  let buf =
	    String.make n ' ' in
	    ignore @@ Unix.recv sock buf 0 n [];
	    buf in
	let size =
	  bytes_of_string (recv 4) in
	  Marshal.from_string (recv size) 0


  let socket_with host port f =
    let s =
      Unix.socket PF_INET SOCK_STREAM 0 in
    let { ai_addr } =
      List.hd @@ getaddrinfo host (string_of_int port) [] in
      f s ai_addr

  let connect ~host ~port =
    socket_with host port begin fun s addr ->
      connect s addr;
      s
    end

  let server ~host ~port ~f =
    socket_with host port begin fun s addr ->
      bind   s addr;
      listen s 1;
      at_exit (fun () -> shutdown s SHUTDOWN_ALL);
      while true do
	let (client, _) =
	  accept s in
	  ignore @@ Thread.create (fun () -> f client) ()
      done
    end
end

include M
