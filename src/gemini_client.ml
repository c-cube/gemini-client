
type code = int
let spf = Printf.sprintf

module Out = struct
  class virtual t = object (self)
    method virtual write : bytes -> int -> int -> unit
    method virtual close : unit

    method write_s s =
      self#write (Bytes.unsafe_of_string s) 0 (String.length s)
  end

  class base (sock:Ssl.socket) = object (self)
    inherit t
    method close = Ssl.shutdown_connection sock
    method write s i len =
      if len > 0 then (
        let n = Ssl.write sock s i len in
        self#write s (i+n) (len-n)
      ) else (
        Ssl.flush sock
      )
  end
end

module In = struct
  class virtual t = object (self)
    method virtual peek : bytes * int * int
    method virtual consume : int -> unit
    method virtual close : unit

    method read_line : string =
      let buf = Buffer.create 32 in
      let continue = ref true in
      while !continue do
        let s, i, len = self#peek in
        match Bytes.index_from s i '\n' with
        | exception Not_found ->
          Buffer.add_subbytes buf s i len;
          self#consume len
        | j when j > i+len ->
          Buffer.add_subbytes buf s i len;
          self#consume len
        | j ->
          Buffer.add_subbytes buf s i (j-i);
          self#consume (1+j-i);
          continue := false
      done;
      Buffer.contents buf

    method read_all : string =
      let buf = Buffer.create 32 in
      let continue = ref true in
      while !continue do
        let s, i, len = self#peek in
        if len<=0 then (
          continue := false
        ) else (
          Buffer.add_subbytes buf s i len;
          self#consume len
        )
      done;
      Buffer.contents buf
  end

  class base (sock:Ssl.socket) = object (self)
    inherit t
    val buf = Bytes.create (16 * 1024)
    val mutable closed = false
    val mutable i = 0
    val mutable len = 0
    method refill_ =
      i <- 0;
      try
        len <- Ssl.read sock buf 0 (Bytes.length buf);
        Printf.eprintf "read %d bytes\n%!" len;
        if len = 0 then closed <- true
      with _ ->
        Printf.eprintf "ssl err: %s\n%!" (Ssl.get_error_string());
        closed <- true
    method close = Ssl.shutdown_connection sock
    method peek =
      if len=0 && not closed then (
        self#refill_
      );
      buf, i, len
    method consume n =
      Printf.eprintf "consume %d bytes\n%!" n;
      assert (n <= len);
      i <- i + n;
      len <- len - n
  end
end

let getopt f = function
  | None -> f()
  | Some x -> x

let rec get ?(max_redirects=10) (url:Uri.t) : (code * string * string, code * string) result =
  try
    let host =
      Uri.host url |> getopt (fun () -> failwith "host is required")
    and port =
      Uri.port url |> getopt (fun () -> 1965)
    in
    (* resolve *)
    let ip_addr = match Unix.getaddrinfo host "" [] with
      | {Unix.ai_addr=Unix.ADDR_INET (ip,_);_} :: _ -> ip
      | _ -> failwith "cannot resolve address"
    in

    let ssl_ctx = Ssl.create_context Ssl.TLSv1_3 Ssl.Client_context in
    Ssl.set_client_verify_callback_verbose true;
    Ssl.set_verify ssl_ctx [] None;
    (* TODO:
        Ssl.set_verify ssl_ctx [Ssl.Verify_peer; Ssl.Verify_fail_if_no_peer_cert]
          (Some Ssl.client_verify_callback);
    *)

    Printf.printf "opening sock to %s:%d...\n%!" (Unix.string_of_inet_addr ip_addr) port;
    let sock =
      Ssl.open_connection_with_context ssl_ctx
        (Unix.ADDR_INET (ip_addr, port))
    in
    Printf.printf "sock created\n%!";
    (* TODO
    Ssl.verify sock;
    Printf.printf "sock verified\n%!";
    *)
    Ssl.connect sock;
    Printf.printf "sock connected\n%!";

    (* now send query *)
    let oc = new Out.base sock in
    oc#write_s (spf "%s\r\n" (Uri.to_string url));
    Printf.printf "sent query\n%!";

    let ic = new In.base sock in

    let line = ic#read_line in
    Printf.printf "got line %S\n%!" line;
    let code, meta =
      try
        let i =
          try String.index line ' '
          with Not_found -> String.index line '\t'
        in
        int_of_string (String.sub line 0 i),
        String.trim @@ String.sub line (i+1) (String.length line-i-1)
      with _ ->
        Ssl.shutdown_connection sock;
        failwith @@ spf "invalid header line %S" line
    in
    if code >= 20 && code < 30 then (
      let body = ic#read_all in
      Ssl.shutdown_connection sock;
      Ok (code, meta, body)
    ) else if code >= 30 && code < 40 then (
      if max_redirects > 0 then (
        let url' = Uri.of_string meta in
        Printf.eprintf "redirect (code %d) to %S\n%!" code (Uri.to_string url');
        get ~max_redirects:(max_redirects-1) url'
      ) else (
        Error (code, spf "max number of redirects reached (redirect to %s)" meta)
      )
    ) else (
      Ssl.shutdown_connection sock;
      Error (code, meta)
    )
  with
  | Failure e -> Error (40, e)
  | e -> Error (40, Printexc.to_string e)


