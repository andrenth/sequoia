open Printf
open Sequoia_common

module Driver = Sequoia_driver
module Vector = Sequoia_vector
module Nat    = Vector.Nat

module type NAMED = sig
  type t
  val name : string
end

module Make (D : Driver.S) = struct
  module Param  = Sequoia_param
  module Lit    = Sequoia_lit
  module Expr   = Sequoia_expr
  module Table  = Sequoia_table
  module Field  = Sequoia_field
  module Vector = Sequoia_vector

  module type TABLE = sig
    type t
    val table : t Table.t

    module Field : sig
      type table = t
      type 'a t = (table, 'a) Field.t
      type 't foreign_key = (table, 't) Field.foreign_key

      val bool : string -> bool t
      val int : string -> int t
      val float : string -> float t
      val string : string -> string t
      val blob : string -> bytes t

      val foreign_key : string -> references:('t, int) Field.t -> 't foreign_key

      module Null : sig
        val bool : string -> bool option t
        val int : string -> int option t
        val float : string -> float option t
        val string : string -> string option t
        val blob : string -> bytes option t
      end
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
    (module struct
      include MakeTable (struct type t let name = name end)
    end : TABLE)

  module Select = Sequoia_select.Make (D)
  module Insert = Sequoia_insert.Make (D)
  module Update = Sequoia_update.Make (D)
  module Delete = Sequoia_delete.Make (D)
end
