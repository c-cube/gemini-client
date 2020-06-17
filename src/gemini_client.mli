
type code = int

val get :
  ?max_redirects:int ->
  Uri.t -> (code * string * string, code * string) result
(** Get the resource at given URL. *)
