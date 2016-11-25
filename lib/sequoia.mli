module type NAMED = sig
  type t
  val name : string
end

module type S = sig
  module Param  : module type of Sequoia_param
  module Lit    : module type of Sequoia_lit
  module Expr   : module type of Sequoia_expr
  module Table  : module type of Sequoia_table
  module Field  : module type of Sequoia_field
  module Vector : module type of Sequoia_vector

  module Select  : Sequoia_select.S
  module Insert  : Sequoia_insert.S
  module Replace : Sequoia_replace.S
  module Update  : Sequoia_update.S
  module Delete  : Sequoia_delete.S

  module type NULL_FIELD = sig
    type 'a t
    val bool : string -> bool option t
    val int : string -> int option t
    val float : string -> float option t
    val string : string -> string option t
    val blob : string -> bytes option t
  end

  module type FIELD = sig
    type table
    type 'a t = (table, 'a) Field.t
    type 't foreign_key = (table, 't) Field.foreign_key

    val bool : string -> bool t
    val int : string -> int t
    val float : string -> float t
    val string : string -> string t
    val blob : string -> bytes t

    val foreign_key : string -> references:('t, int) Field.t -> 't foreign_key
  end

  module type TABLE = sig
    type t
    val table : t Table.t

    module Field : sig
      include FIELD with type table = t
      module Null : NULL_FIELD with type 'a t := 'a t
    end
  end

  module MakeTable (T: NAMED) : TABLE
end

module Make (D : Sequoia_driver.S) : S
  with type 't Table.t = 't Sequoia_table.t
   and type ('t, 'a) Field.t = ('t, 'a) Sequoia_field.t
   and type ('t1, 't2) Field.foreign_key = ('t1, 't2) Sequoia_field.foreign_key
   and type 'a Lit.t = 'a Sequoia_lit.t
   and type 'a Expr.t = 'a Sequoia_expr.t
   and type 'a Expr.cast = 'a Sequoia_expr.cast
   and type Expr.handover = Sequoia_expr.handover
   and type Param.t = Sequoia_param.t
   and module Lit.Vector = Sequoia_lit.Vector
