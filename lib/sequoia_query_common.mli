(** Definitions useful in multiple query types. *)

val build_where : placeholder:(int -> string)
               -> handover:Sequoia_expr.handover
               -> Sequoia_common.build_step
               -> 'a Sequoia_expr.t option
               -> Sequoia_common.build_step
  (** Build a [WHERE] expression. *)

val build_limit : (int -> string)
               -> Sequoia_common.build_step
               -> (int * int) option
               -> Sequoia_common.build_step
  (** Build a [LIMIT] expression. *)

(** Expressions to be used in [UPDATE] or [DELETE] queries. *)
module UpdateDeleteExpr : sig
  type _ Sequoia_expr.t +=
    | Field : ('t, 'a) Sequoia_field.t * 't Sequoia_table.t -> 'a Sequoia_expr.t
    | Foreign : ('t, 'u) Sequoia_field.foreign_key * 't Sequoia_table.t
             -> 'a Sequoia_expr.t
    (** Extend the basic expressions with fields and foreign keys. *)

  val field : ('t, 'a) Sequoia_field.t -> 't Sequoia_table.t
           -> 'a Sequoia_expr.t
    (** A table field expression. *)

  val foreign_key : ('t1, 't2) Sequoia_field.foreign_key
                 -> 't1 Sequoia_table.t
                 -> 'a Sequoia_expr.t
    (** A table foreign key expression. *)

  (** {3 Functions useful for driver writers} *)

  val build : placeholder:(int -> string)
           -> handover:Sequoia_expr.handover
           -> Sequoia_common.build_step
           -> 'a Sequoia_expr.t
           -> Sequoia_common.build_step
end

(** Types and values used in [INSERT] and [REPLACE] queries. They're not
    supposed to be called directly and therefore are not documented here. *)
module InsertReplace : sig
  module Vector : Sequoia_vector.S
    with type ('t, 'a) elem := ('t, 'a) Sequoia_field.t

  type _ t

  val create : into:'t Sequoia_table.t
            -> fields:('t, 'a, 'n Sequoia_vector.Nat.s) Vector.t
            -> values:('u, 'a, 'm Sequoia_vector.Nat.s, 'n Sequoia_vector.Nat.s)
                      Sequoia_lit.Vector.matrix
            -> 't t

  val seal : placeholder:(int -> string)
          -> query:string
          -> 't t
          -> string * Sequoia_param.t list
end
