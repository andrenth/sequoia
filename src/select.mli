(** [SELECT] query builder. *)

open Common

module type S = sig
  type _ t
    (** The type of [SELECT] queries. *)

  type ('s1, 't1, 't2, 's2) join_steps =
    | There : (('t2 -> _) as 's1, 't1, 't2, 't1 -> 's1) join_steps
    | Skip : ('s1, 't1, 't2, 's2) join_steps
          -> ('a -> 's1, 't1, 't2, 'a -> 's2) join_steps
    (** This type allows the representation of SQL joins in the OCaml type
        system. *)

  type join
    (** The type of SQL joins. *)

  type 's source =
    | From : 't Table.t -> ('t -> unit) source
    | Join : join
           * ('t1, 't2) Field.foreign_key
           * 's1 source
           * ('s1, 't1, 't2, 's2) join_steps
          -> 's2 source
    (** Data sources for [SELECT] queries: tables specified in the the [FROM]
        or [JOIN] clause. Joins are only allowed on fields declared as
        foreign keys. The [join_steps] type joins the tables in the OCaml
        type system sense. *)

  type ('s1, 't1, 't2, 's2) steps =
    | There : (('t2 -> _) as 's1, 't1, 't2, 's1) steps
    | Skip : ('s1, 't1, 't2, 's2) steps
          -> ('a -> 's1, 't1, 't2, 'a -> 's2) steps
    (** This is analogous to [join_steps] but instead of building the set of
        data sources (i.e. the tables being referenced in the query), it is
        used to simply reference them. *)

  val seal : handover:Expr.handover
          -> 's t
          -> string * Param.t list
		(** Mark the end of a [SELECT] query. *)

  (** Expressions that are valid in [SELECT] queries. *)
  module Expr : sig
    type 's select = 's t

    type 'a Expr.t +=
      | Field : ('t, 'a) Field.t
              * 's1 source
              * ('s1, 't1, 't, 's2) steps
             -> 'a Expr.t
      | Foreign : ('t, 't2) Field.foreign_key
                * 's1 source
                * ('s1, 't1, 't, 's2) steps
               -> 'a Expr.t
      | Select : 's select -> 'a Expr.t
      (** Extend basic expressions with field and foreign key references and
          subqueries. *)

    type 'a t = 'a Expr.t

    val (-->) : ('s source -> 'a t) -> string -> 's source -> 'a t
      (** Define an alias for an expression (i.e. the SQL [AS] keyword). *)

    val alias : string -> 's source -> 'a t
      (** Reference a previously defined alias. Note: alias references are
          not type-safe! *)

    val field : ('t, 'a) Field.t
             -> ('b, 'c, 't, 'd) steps
             -> 'b source
             -> 'a t
      (** A field expression. *)

    val foreign_key : ('t1, 't2) Field.foreign_key
                   -> ('a, 'b, 't1, 'c) steps
                   -> 'a source
                   -> 'd t
      (** A foreign key expression. *)

    val subquery : 's select -> 't source -> 'c t
      (** A subquery expression. *)

    val unwrap : ('t, 'a option) Field.t
              -> ('b, 'c, 't, 'd) steps
              -> 'b source
              -> 'a t
      (** Convert a nullable field to an expression of the non-null type. *)

    type ('s, 'a) mk = 's source -> 'a t
      (** An expression builder, i.e. a function that returns an expression
          when given a [source]. *)

    module Vector : Vector.S with type ('s, 'a) elem := ('s, 'a) mk
      (** Vectors of expression builders. *)

    (** {3 Functions useful for driver writers} *)

    val build : handover:Expr.handover -> build_step -> 'a t
             -> build_step
      (** Build an expression. *)

    val vectormk_to_vector : 's source -> ('s, 'a, 'n) Vector.t
                          -> ('s, 'a, 'n) Expr.Vector.t
      (** Convert a vector of expression builders into a vector of expressions
          when given a [source]. *)
  end

  val select : ?distinct:bool
            -> ('s, 'a, 'n Nat.s) Expr.Vector.t
            -> 's source
            -> 's t
    (** Define the selected expressions in a [SELECT] query. *)

  val from : 't Table.t -> ('t -> unit) source
    (** Starts a [SELECT] query by specifying the initial data source. *)

  val left_join  : ('t1, 't2) Field.foreign_key *
                   ('s1, 't1, 't2, 's2) join_steps
                -> 's1 source
                -> 's2 source
    (** Specifies a [LEFT JOIN] adding the table containing the given foreign
        key as a data source. *)

  val right_join : ('t1, 't2) Field.foreign_key *
                   ('s1, 't1, 't2, 's2) join_steps
                -> 's1 source
                -> 's2 source
    (** Specifies a [RIGHT JOIN] adding the table containing the given foreign
        key as a data source. *)

  val inner_join : ('t1, 't2) Field.foreign_key *
                   ('s1, 't1, 't2, 's2) join_steps
                -> 's1 source
                -> 's2 source
    (** Specifies an [INNER JOIN] adding the table containing the given foreign
        key as a data source. *)

  val self : ('t, int) Field.t
          -> ('t, int) Field.t
          -> ('s1, 't1, 't, 's2) join_steps
          -> ('t, 't) Field.foreign_key * ('s1, 't1, 't, 's2) join_steps
    (** Allows self-joins, i.e. joins that use fields belonging to a single
        table. *)

  val this : ('t1, 't2) Field.foreign_key
          -> ('s1, 't1, 't2, 's2) join_steps
          -> ('t1, 't2) Field.foreign_key *
             ('s1, 't1, 't2, 's2) join_steps
    (** Call this on a foreign key when you want to define a join with the
        table that contains the foreign key. *)

  val that : ('t1, 't2) Field.foreign_key
          -> ('s1, 't2, 't1, 's2) join_steps
          -> ('t2, 't1) Field.foreign_key *
             ('s1, 't2, 't1, 's2) join_steps
    (** Call this on a foreign key when you want to define a join with the
        table that contains the field that the foreign key references. *)

  val where : ('a source -> bool Expr.t) -> 'a t -> 'a t
    (** Defines a [WHERE] clause for a [SELECT] query. *)

  val group_by : ?having:('s source -> bool Expr.t)
              -> ('s, 'a, 'n Nat.s) Expr.Vector.t
              -> 's t
              -> 's t
    (** Defines a [GROUP BY] clause for a [SELECT] query. *)

  (** Definitions for [ORDER BY] clauses. *)
  module OrderBy : sig
    type order
      (** The order type, i.e. ascending or descending. *)

    type ('s, 'a) expr = 'a Expr.t * order
      (** Expressions in [ORDER BY] clauses must specify an order. *)

    module Expr : sig
      type ('s, 'a) mk = 's source -> 'a Expr.t * order
        (** Type of [ORDER BY] expressions builders. *)

      module Vector : Vector.S with type ('s, 'a) elem := ('s, 'a) mk
        (** Vector of [ORDER BY] expressions builders. *)

      val asc : ('s source -> 'a Expr.t) -> 's source -> ('s, 'a) expr
        (** Order by the given expression ascendingly. *)

      val desc : ('s source -> 'a Expr.t) -> 's source -> ('s, 'a) expr
        (** Order by the given expression descendingly. *)
    end

    module Vector : Vector.S with type ('s, 'a) elem := ('s, 'a) expr
      (** Vector of [ORDER BY] expressions. *)

    (** {3 Functions useful for driver writers} *)

    val vectormk_to_vector : 's source -> ('s, 'a, 'n) Expr.Vector.t
                          -> ('s, 'a, 'n) Vector.t
      (** Convert a vector of expression builders into a vector of expressions
          when given a [source]. *)
  end

  val order_by : ('s, 'a, 'n Nat.s) OrderBy.Expr.Vector.t
              -> 's t
              -> 's t
    (** Defines an [ORDER BY] clause in a [SELECT] query. *)

  val limit : ?offset:int -> int -> 'a t -> 'a t
    (** Defines a [LIMIT] clause in a [SELECT] query. *)
end

module Make (D : Driver.S) : S
	(** Functor that generates a [SELECT] query builder for the given driver. *)
