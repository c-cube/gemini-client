
type code = int

val get : Uri.t -> (code * string * string, code * string) result
(** Get the resource at given URL. *)
