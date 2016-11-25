(** [REPLACE] query builder *)

module type S = sig
  module Vector :
    Sequoia_vector.S with type ('t, 'a) elem := ('t, 'a) Sequoia_field.t
    (** Vector of fields. *)

  type _ t
    (** The type of [REPLACE] queries. *)

  val replace : into:'t Sequoia_table.t
             -> fields:('t, 'a, 'n Sequoia_vector.Nat.s) Vector.t
             -> values:
                 ('u, 'a, 'm Sequoia_vector.Nat.s, 'n Sequoia_vector.Nat.s)
                 Sequoia_lit.Vector.matrix
             -> 't t
    (** Create a [REPLACE] query for table [into], setting the given [fields]
        to the respective [values]. *)

  val seal : 't t -> string * Sequoia_param.t list
		(** Mark the end of a [REPLACE] query. *)
end

module Make (D : Sequoia_driver.S) : S
	(** Functor that generates a [REPLACE] query builder for the given driver. *)
