(** [REPLACE] query builder *)

module type S = sig
  module Vector :
    Vector.S with type ('t, 'a) elem := ('t, 'a) Field.t
    (** Vector of fields. *)

  type _ t
    (** The type of [REPLACE] queries. *)

  val replace : into:'t Table.t
             -> fields:('t, 'a, 'n Nat.s) Vector.t
             -> values:
                 ('u, 'a, 'm Nat.s, 'n Nat.s)
                 Lit.Vector.matrix
             -> 't t
    (** Create a [REPLACE] query for table [into], setting the given [fields]
        to the respective [values]. *)

  val seal : 't t -> string * Param.t list
		(** Mark the end of a [REPLACE] query. *)
end

module Make (D : Driver.S) : S
	(** Functor that generates a [REPLACE] query builder for the given driver. *)
