(** Sequoia is a type-safe query builder for OCaml.*)

module type NAMED = sig
  type t
  val name : string
end

(** The Sequoia query builder interface, output by the [Make] functor below. *)
module type S = sig
  (** {2 Query builder interface} *)

  module Param  : module type of Sequoia_param
    (** Query parameters. *)
  module Lit    : module type of Sequoia_lit
    (** Literal values. *)
  module Expr   : module type of Sequoia_expr
    (** Expressions. *)
  module Table  : module type of Sequoia_table
    (** SQL tables. *)
  module Field  : module type of Sequoia_field
    (** Field definitions. *)
  module Vector : module type of Sequoia_vector
    (** Heterogeneous lists with type-encoded length. *)

  module Select  : Sequoia_select.S
    (** [SELECT] queries. *)
  module Insert  : Sequoia_insert.S
    (** [INSERT] queries. *)
  module Replace : Sequoia_replace.S
    (** [REPLACE] queries. *)
  module Update  : Sequoia_update.S
    (** [UPDATE] queries. *)
  module Delete  : Sequoia_delete.S
    (** [DELETE] queries. *)

	(** Field definitions for a given table. *)
  module type FIELD = sig
    type table
    type 'a t = (table, 'a) Field.t
    type 't foreign_key = (table, 't) Field.foreign_key

		(** {3 Field definition functions}

		    These functions define a field of the respective type that
        corresponds to the SQL field given by the string parameter. *)

    val bool : string -> bool t
    val int : string -> int t
    val float : string -> float t
    val string : string -> string t
    val blob : string -> bytes t

    val foreign_key : string -> references:('t, int) Field.t -> 't foreign_key
  end

	(** Nullable field definitions for a given table. *)
  module type NULL_FIELD = sig
    type 'a t

		(** {3 Field definition functions}

		    These functions define a field of the respective type that
        corresponds to the nullable SQL field given by the string parameter. *)

    val bool : string -> bool option t
    val int : string -> int option t
    val float : string -> float option t
    val string : string -> string option t
    val blob : string -> bytes option t
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
		(** [table name] returns a module that can be used to define SQL table
				fields. The intended use is for this result to be included in an
			  OCaml module that will hold the field definitions. *)

  module MakeTable (T: NAMED) : TABLE
		(** Functor that generates a table module to be used in SQL table
        defintions. Should only be useful for driver writers. *)
end

(* Functor that outputs a query builder from  *)
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
