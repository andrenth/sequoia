val build_where : placeholder:(int -> string)
               -> handover:Sequoia_expr.handover
               -> Sequoia_common.build_step
               -> 'a Sequoia_expr.t option
               -> Sequoia_common.build_step

val build_limit : (int -> string)
               -> Sequoia_common.build_step
               -> (int * int) option
               -> Sequoia_common.build_step


module UpdateDeleteExpr : sig
  type _ Sequoia_expr.t +=
    | Field : ('t, 'a) Sequoia_field.t * 't Sequoia_table.t -> 'a Sequoia_expr.t
    | Foreign : ('t, 'u) Sequoia_field.foreign_key * 't Sequoia_table.t
             -> 'a Sequoia_expr.t

  val field : ('t, 'a) Sequoia_field.t -> 't Sequoia_table.t
           -> 'a Sequoia_expr.t
  val foreign_key : ('t1, 't2) Sequoia_field.foreign_key
                 -> 't1 Sequoia_table.t
                 -> 'a Sequoia_expr.t

  val build : placeholder:(int -> string)
           -> handover:Sequoia_expr.handover
           -> Sequoia_common.build_step
           -> 'a Sequoia_expr.t
           -> Sequoia_common.build_step
end
