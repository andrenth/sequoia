(** Definitions useful in multiple query types. *)

val build_where : placeholder:(int -> string)
               -> handover:Expr.handover
               -> Common.build_step
               -> 'a Expr.t option
               -> Common.build_step
  (** Build a [WHERE] expression. *)

val build_limit : (int -> string)
               -> Common.build_step
               -> (int * int) option
               -> Common.build_step
  (** Build a [LIMIT] expression. *)

(** Expressions to be used in [UPDATE] or [DELETE] queries. *)
module UpdateDeleteExpr : sig
  type _ Expr.t +=
    | Field : ('t, 'a) Field.t * 't Table.t -> 'a Expr.t
    | Foreign : ('t, 'u) Field.foreign_key * 't Table.t
             -> 'a Expr.t
    (** Extend the basic expressions with fields and foreign keys. *)

  val field : ('t, 'a) Field.t -> 't Table.t
           -> 'a Expr.t
    (** A table field expression. *)

  val foreign_key : ('t1, 't2) Field.foreign_key
                 -> 't1 Table.t
                 -> 'a Expr.t
    (** A table foreign key expression. *)

  (** {3 Functions useful for driver writers} *)

  val build : placeholder:(int -> string)
           -> handover:Expr.handover
           -> Common.build_step
           -> 'a Expr.t
           -> Common.build_step
end

(** Types and values used in [INSERT] and [REPLACE] queries. They're not
    supposed to be called directly and therefore are not documented here. *)
module InsertReplace : sig
  module Vector : Vector.S
    with type ('t, 'a) elem := ('t, 'a) Field.t

  type _ t

  val create : into:'t Table.t
            -> fields:('t, 'a, 'n Nat.s) Vector.t
            -> values:('u, 'a, 'm Nat.s, 'n Nat.s)
                      Lit.Vector.matrix
            -> 't t

  val seal : placeholder:(int -> string)
          -> query:string
          -> 't t
          -> string * Param.t list
end
