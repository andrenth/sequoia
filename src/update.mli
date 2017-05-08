(** [UPDATE] query builder. *)

module type S = sig
  type _ t
    (** The type of [UPDATE] queries. *)

  val seal : handover:Expr.handover -> 't t
          -> string * Param.t list
		(** Mark the end of a [UPDATE] query. *)

  (** Definitions for [ORDER BY] clauses. *)
  module OrderBy : sig
    type order
      (** The order type, i.e. ascending or descending. *)

    type ('t, 'a) expr = 'a Expr.t * order
      (** Expressions in [ORDER BY] clauses must specify an order. *)

    module Expr : sig
      type ('t, 'a) mk = 't Table.t -> 'a Expr.t * order
        (** Type of [ORDER BY] expressions builders. *)

      module Vector : Vector.S with type ('t, 'a) elem := ('t, 'a) mk
        (** Vector of [ORDER BY] expressions builders. *)

      val asc : ('t Table.t -> 'a Expr.t)
             -> 't Table.t
             -> ('t, 'a) expr
        (** Order by the given expression ascendingly. *)

      val desc : ('t Table.t -> 'a Expr.t)
              -> 't Table.t
              -> ('t, 'a) expr
        (** Order by the given expression descendingly. *)
    end

    module Vector : Vector.S with type ('t, 'a) elem := ('t, 'a) expr
      (** Vector of [ORDER BY] expressions. *)

    (** {3 Functions useful for driver writers} *)

    val vectormk_to_vector : 't Table.t
                          -> ('t, 'a, 'n) Expr.Vector.t
                          -> ('t, 'a, 'n) Vector.t
      (** Convert a vector of expression builders into a vector of expressions
          when given a [source]. *)
  end

  type ('t, 'a) mk = ('t, 'a) Field.t * ('t Table.t -> ('t, 'a) Expr.expr)
    (** The type of [UPDATE] builders. *)

  val where : ('t Table.t -> 'a Expr.t) -> 't t -> 't t
    (** Defines a [WHERE] clause for an [UPDATE] query. *)

  val order_by : ('t, 'a, 'n Nat.s) OrderBy.Expr.Vector.t
              -> 't t
              -> 't t
    (** Defines an [ORDER BY] clause for an [UPDATE] query. *)

  val limit : ?offset:int -> int -> 't t -> 't t
    (** Defines a [LIMIT] clause for an [UPDATE] query. *)

  (** [UPDATE] query expressions. *)
  module Expr : sig
    include module type of Query_common.UpdateDeleteExpr

    type ('t, 'a) mk = 't Table.t -> ('t, 'a) Expr.expr
      (** The type of [UPDATE] expression builders. *)

    module Vector : Vector.S with type ('t, 'a) elem := ('t, 'a) mk
      (** Vector of [UPDATE] expression builders. *)
  end

  module Vector : Vector.S with type ('t, 'a) elem := ('t, 'a) mk
    (** Vector of [UPDATE] builders. *)

  val update : 't Table.t
            -> set:('t, 'a, 'n Nat.s) Vector.t
            -> 't t
    (** Defines an [UPDATE] query for the given field/expression pairs. *)
end

module Make (D : Driver.S) : S
	(** Functor that generates an [UPDATE] query builder for the given driver. *)
