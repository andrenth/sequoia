module type S = sig
  module Vector :
    Sequoia_vector.S with type ('t, 'a) elem := ('t, 'a) Sequoia_field.t

  type _ t

  val insert : into:'t Sequoia_table.t
            -> fields:('t, 'a, 'n Sequoia_vector.Nat.s) Vector.t
            -> values:
                ('u, 'a, 'm Sequoia_vector.Nat.s, 'n Sequoia_vector.Nat.s)
                Sequoia_lit.Vector.matrix
            -> 't t

  val seal : 't t -> string * Sequoia_param.t list
end

module Make (D : Sequoia_driver.S) : S
