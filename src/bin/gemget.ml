
let get (url:Uri.t) : unit =
  match Gemini_client.get url with
  | Ok (code, meta, x) ->
    Printf.printf "got %s (code %d):\n%s\n" meta code x
  | Error (code, e) ->
    Printf.printf "error (code %d): %s\n%!" code e;
    exit 1


let () =
  let url = ref "" in
  let opts = [

  ] |> Arg.align in
  Arg.parse opts (fun x -> url := x) "gemget <url>";
  if !url = "" then (
    failwith "please provide a url";
  );
  match Uri.of_string !url with
  | u -> get u
  | exception e ->
    Printf.printf "invalid uri: %s" (Printexc.to_string e);
    exit 1
