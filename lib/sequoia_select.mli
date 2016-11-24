open Sequoia_common

module type S = sig
  type _ t

  type ('s1, 't1, 't2, 's2) join_steps =
    | There : (('t2 -> _) as 's1, 't1, 't2, 't1 -> 's1) join_steps
    | Skip : ('s1, 't1, 't2, 's2) join_steps
          -> ('a -> 's1, 't1, 't2, 'a -> 's2) join_steps

  type join

  type 's source =
    | From : 't Sequoia_table.t -> ('t -> unit) source
    | Join : join
           * ('t1, 't2) Sequoia_field.foreign_key
           * 's1 source
           * ('s1, 't1, 't2, 's2) join_steps
          -> 's2 source

  type ('s1, 't1, 't2, 's2) steps =
    | There : (('t2 -> _) as 's1, 't1, 't2, 's1) steps
    | Skip : ('s1, 't1, 't2, 's2) steps
          -> ('a -> 's1, 't1, 't2, 'a -> 's2) steps

  val seal : handover:Sequoia_expr.handover
          -> 's t
          -> string * Sequoia_param.t list

  module Expr : sig
    type 's select = 's t

    type 'a Sequoia_expr.t +=
      | Field : ('t, 'a) Sequoia_field.t
              * 's1 source
              * ('s1, 't1, 't, 's2) steps
             -> 'a Sequoia_expr.t
      | Foreign : ('t, 't2) Sequoia_field.foreign_key
                * 's1 source
                * ('s1, 't1, 't, 's2) steps
               -> 'a Sequoia_expr.t
      | Select : 's select -> 'a Sequoia_expr.t

    type 'a t = 'a Sequoia_expr.t

    val build : handover:Sequoia_expr.handover -> build_step -> 'a t
             -> build_step

    val (-->) : ('s source -> 'a t) -> string -> 's source -> 'a t

    val alias : string -> 's source -> 'a t

    val field : ('t, 'a) Sequoia_field.t
             -> ('b, 'c, 't, 'd) steps
             -> 'b source
             -> 'a t

    val foreign_key : ('t1, 't2) Sequoia_field.foreign_key
                   -> ('a, 'b, 't1, 'c) steps
                   -> 'a source
                   -> 'd t

    val subquery : 's select -> 't source -> 'c t

    val unwrap : ('t, 'a option) Sequoia_field.t
              -> ('b, 'c, 't, 'd) steps
              -> 'b source
              -> 'a t

    type ('s, 'a) mk = 's source -> 'a t

    module Vector : Sequoia_vector.S with type ('s, 'a) elem := ('s, 'a) mk

    val vectormk_to_vector : 's source -> ('s, 'a, 'n) Vector.t
                          -> ('s, 'a, 'n) Sequoia_expr.Vector.t
  end

  val select : ?distinct:bool
            -> ('s, 'a, 'n Sequoia_vector.Nat.s) Expr.Vector.t
            -> 's source
            -> 's t

  val from : 't Sequoia_table.t -> ('t -> unit) source

  val left_join  : ('t1, 't2) Sequoia_field.foreign_key *
                   ('s1, 't1, 't2, 's2) join_steps
                -> 's1 source
                -> 's2 source
  val right_join : ('t1, 't2) Sequoia_field.foreign_key *
                   ('s1, 't1, 't2, 's2) join_steps
                -> 's1 source
                -> 's2 source
  val inner_join : ('t1, 't2) Sequoia_field.foreign_key *
                   ('s1, 't1, 't2, 's2) join_steps
                -> 's1 source
                -> 's2 source

  val self : ('t, int) Sequoia_field.t
          -> ('t, int) Sequoia_field.t
          -> ('s1, 't1, 't, 's2) join_steps
          -> ('t, 't) Sequoia_field.foreign_key * ('s1, 't1, 't, 's2) join_steps

  val this : ('t1, 't2) Sequoia_field.foreign_key
          -> ('s1, 't1, 't2, 's2) join_steps
          -> ('t1, 't2) Sequoia_field.foreign_key *
             ('s1, 't1, 't2, 's2) join_steps
  val that : ('t1, 't2) Sequoia_field.foreign_key
          -> ('s1, 't2, 't1, 's2) join_steps
          -> ('t2, 't1) Sequoia_field.foreign_key *
             ('s1, 't2, 't1, 's2) join_steps

  val where : ('a source -> bool Expr.t) -> 'a t -> 'a t
  val group_by : ?having:('s source -> bool Expr.t)
              -> ('s, 'a, 'n Sequoia_vector.Nat.s) Expr.Vector.t
              -> 's t
              -> 's t

  module OrderBy : sig
    type order

    type ('s, 'a) expr = 'a Expr.t * order

    module Expr : sig
      type ('s, 'a) mk = 's source -> 'a Expr.t * order

      module Vector : Sequoia_vector.S with type ('s, 'a) elem := ('s, 'a) mk

      val asc : ('s source -> 'a Expr.t) -> 's source -> ('s, 'a) expr
      val desc : ('s source -> 'a Expr.t) -> 's source -> ('s, 'a) expr
    end

    module Vector : Sequoia_vector.S with type ('s, 'a) elem := ('s, 'a) expr

    val vectormk_to_vector : 's source -> ('s, 'a, 'n) Expr.Vector.t
                          -> ('s, 'a, 'n) Vector.t
  end

  val order_by : ('s, 'a, 'n Sequoia_vector.Nat.s) OrderBy.Expr.Vector.t
              -> 's t
              -> 's t

  val limit : ?offset:int -> int -> 'a t -> 'a t
end

module Make (D : Sequoia_driver.S) : S
