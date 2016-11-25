open Printf
open Sequoia_common

module Driver = Sequoia_driver
module Vector = Sequoia_vector
module Nat    = Vector.Nat

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

	val table : string -> (module TABLE)

  module MakeTable (T: NAMED) : TABLE
end

module Make (D : Driver.S) : S
  with type 't Table.t = 't Sequoia_table.t
   and type ('t, 'a) Field.t = ('t, 'a) Sequoia_field.t
   and type ('t1, 't2) Field.foreign_key = ('t1, 't2) Sequoia_field.foreign_key
   and type 'a Lit.t = 'a Sequoia_lit.t
   and type 'a Expr.t = 'a Sequoia_expr.t
   and type 'a Expr.cast = 'a Sequoia_expr.cast
   and type Expr.handover = Sequoia_expr.handover
   and type Param.t = Sequoia_param.t
   and module Lit.Vector = Sequoia_lit.Vector =
struct
  module Param  = Sequoia_param
  module Lit    = Sequoia_lit
  module Expr   = Sequoia_expr
  module Table  = Sequoia_table
  module Field  = Sequoia_field
  module Vector = Sequoia_vector

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

  module MakeTable (T: NAMED) : TABLE = struct
    type t = T.t

    let table = Table.called T.name

    module Field = struct
      type table = t
      type 'a t = (table, 'a) Field.t
      type 't foreign_key = (table, 't) Field.foreign_key

      let bool = Field.bool table
      let int = Field.int table
      let float = Field.float table
      let string = Field.string table
      let blob = Field.blob table

      let foreign_key name ~references =
        Field.foreign_key table name ~references

      module Null = struct
        let bool = Field.Null.bool table
        let int = Field.Null.int table
        let float = Field.Null.float table
        let string = Field.Null.string table
        let blob = Field.Null.blob table
      end
    end
  end

  let table name =
    (module MakeTable (struct type t let name = name end) : TABLE)

  module Select  = Sequoia_select.Make (D)
  module Insert  = Sequoia_insert.Make (D)
  module Replace = Sequoia_replace.Make (D)
  module Update  = Sequoia_update.Make (D)
  module Delete  = Sequoia_delete.Make (D)
end
