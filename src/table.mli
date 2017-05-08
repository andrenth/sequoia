(** SQL tables *)

type 'a t
  (** The type of tables. *)

val called : string -> 'a t
  (** Define a table with the given name. *)

val name : 'a t -> string
  (** Returns the name of a table. *)
