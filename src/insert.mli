(** [INSERT] query builder. *)

module type S = sig
  module Vector :
    Vector.S with type ('t, 'a) elem := ('t, 'a) Field.t
		(** Vector of fields. *)

  type _ t
		(** The type of [INSERT] queries. *)

  val insert : into:'t Table.t
            -> fields:('t, 'a, 'n Nat.s) Vector.t
            -> values:
                ('u, 'a, 'm Nat.s, 'n Nat.s)
                Lit.Vector.matrix
            -> 't t
		(** Insert the given [values] (given as a vector of vectors) corresponding
        to the appropriate [fields] into the table given by [into]. *)

  val seal : 't t -> string * Param.t list
		(** Mark the end of an [INSERT] query. *)
end

module Make (D : Driver.S) : S
	(** Functor that generates an [INSERT] query builder for the given driver. *)
