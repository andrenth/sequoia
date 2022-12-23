
module type NAMED = sig
  type t
  val name : string
end

module Common = Common

module type S = sig
  module Param  : module type of Param
  module Lit    : module type of Lit
  module Expr   : module type of Expr
  module Table  : module type of Table
  module Field  : module type of Field
  module Vector : module type of Vector

  module Select  : Select.S
  module Insert  : Insert.S
  module Replace : Replace.S
  module Update  : Update.S
  module Delete  : Delete.S

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
  with type 't Table.t = 't Table.t
   and type ('t, 'a) Field.t = ('t, 'a) Field.t
   and type ('t1, 't2) Field.foreign_key = ('t1, 't2) Field.foreign_key
   and type 'a Lit.t = 'a Lit.t
   and type 'a Expr.t = 'a Expr.t
   and type 'a Expr.cast = 'a Expr.cast
   and type Expr.handover = Expr.handover
   and type Param.t = Param.t
   and module Lit.Vector = Lit.Vector =
struct
  module Param  = Param
  module Lit    = Lit
  module Expr   = Expr
  module Table  = Table
  module Field  = Field
  module Vector = Vector

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

  module Select  = Select.Make (D)
  module Insert  = Insert.Make (D)
  module Replace = Replace.Make (D)
  module Update  = Update.Make (D)
  module Delete  = Delete.Make (D)

end
