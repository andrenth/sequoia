(** SQL field definitions. *)

type ('t, 'a) t = ..
	(** The type of SQL fields. Can be extended by driver writers. *)

type ('t, 'a) t +=
  | Bool : string * 't Table.t -> ('t, bool) t
  | Int : string * 't Table.t -> ('t, int) t
  | Float : string * 't Table.t -> ('t, float) t
  | String : string * 't Table.t -> ('t, string) t
  | Blob : string * 't Table.t -> ('t, bytes) t
	(** The basic SQL field types. *)

type ('t1, 't2) foreign_key = ('t1, int) t * ('t2, int) t
	(** The type of foreign keys. They are a different type to allow
      enforcing of field relationships. *)

val name : ('t, 'a) t -> string
	(** The name of a field. *)
val table : ('t, 'a) t -> 't Table.t
	(** The table a field belongs to. *)
val to_string : ('t, 'a) t -> string
	(** The string representation of a field, i.e. [table].[name]. *)

val bool : 't Table.t -> string -> ('t, bool) t
	(** A boolean field. *)
val int : 't Table.t -> string -> ('t, int) t
	(** An integer field. *)
val float : 't Table.t -> string -> ('t, float) t
	(** A float field. *)
val string : 't Table.t -> string -> ('t, string) t
	(** A string field. *)
val blob : 't Table.t -> string -> ('t, bytes) t
	(** A blob field. *)

val foreign_key : 't1 Table.t -> string -> references:('t2, int) t
               -> ('t1, 't2) foreign_key
	(** A foreign key referencing a field in another table. *)

module Null : sig
  type ('t, 'a) t +=
    | Bool : string * 't Table.t -> ('t, bool option) t
    | Int : string * 't Table.t -> ('t, int option) t
    | Float : string * 't Table.t -> ('t, float option) t
    | String : string * 't Table.t -> ('t, string option) t
    | Blob : string * 't Table.t -> ('t, bytes option) t
		(** The type of nullable fields. *)

  val bool : 't Table.t -> string -> ('t, bool option) t
		(** A nullable boolean field. *)
  val int : 't Table.t -> string -> ('t, int option) t
		(** A nullable int field. *)
  val float : 't Table.t -> string -> ('t, float option) t
		(** A nullable float field. *)
  val string : 't Table.t -> string -> ('t, string option) t
		(** A nullable string field. *)
  val blob : 't Table.t -> string -> ('t, bytes option) t
		(** A nullable blob field. *)
end
