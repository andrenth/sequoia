(** [INSERT] query builder. *)

module type S = sig
  module Vector :
    Sequoia_vector.S with type ('t, 'a) elem := ('t, 'a) Sequoia_field.t
		(** Vector of fields. *)

  type _ t
		(** The type of [INSERT] queries. *)

  val insert : into:'t Sequoia_table.t
            -> fields:('t, 'a, 'n Sequoia_vector.Nat.s) Vector.t
            -> values:
                ('u, 'a, 'm Sequoia_vector.Nat.s, 'n Sequoia_vector.Nat.s)
                Sequoia_lit.Vector.matrix
            -> 't t
		(** Insert the given [values] (given as a vector of vectors) corresponding
        to the appropriate [fields] into the table given by [into]. *)

  val seal : 't t -> string * Sequoia_param.t list
		(** Mark the end of an [INSERT] query. *)
end

module Make (D : Sequoia_driver.S) : S
	(** Functor that generates an [INSERT] query builder for the given driver. *)
