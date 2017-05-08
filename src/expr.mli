(** Expressions that (supposedly) exist on every SQL implementation. Everything
    else is left to be implemented in drivers. *)

type 'a t = ..
	(** The type of expressions. It is extended by the query modules (i.e.
			[Select], [Insert], etc.) to account for expressions that are only
      valid for the respective query types. *)
type 'a cast = ..
	(** The type that represents an SQL [CAST()]. *)

(** Module [Base] defines the basic SQL expressions. *)
module Base : sig
  type 'a cast +=
    | Bool : bool cast (** cast to [bool] *)
    | Int : int cast (** cast to [int] *)
    | Float : float cast (** cast to [float] *)
    | String : string cast (** cast to [string] *)
    | Blob : bytes cast (** cast to [bytes] *)

  type 'a t +=
    | Lit : 'a Lit.t -> 'a t
    | Eq : 'a t * 'a t -> bool t
    | Neq : 'a t * 'a t -> bool t
    | Gt : 'a t * 'a t -> bool t
    | Ge : 'a t * 'a t -> bool t
    | Lt : 'a t * 'a t -> bool t
    | Le : 'a t * 'a t -> bool t
    | And : bool t * bool t -> bool t
    | Or : bool t * bool t -> bool t
    | Like : string t * string -> bool t
    | Not_like : string t * string -> bool t
    | In : 'a t * 'a t list -> bool t
    | Not_in : 'a t * 'a t list -> bool t
    | Is_not_null : 'a option t -> bool t
    | Is_null : 'a option t -> bool t
    | IAdd : int t * int t -> int t
    | ISub : int t * int t -> int t
    | IMul : int t * int t -> int t
    | IDiv : int t * int t -> int t
    | FAdd : float t * float t -> float t
    | FSub : float t * float t -> float t
    | FMul : float t * float t -> float t
    | FDiv : float t * float t -> float t
    | LShift : int t * int -> int t
    | RShift : int t * int -> int t
    | Cast : 'a t * 'b cast -> 'b t

	(** {3 Functions corresponding to SQL operators} *)

  val (=)    : ('a -> 'b t) -> ('a -> 'b t) -> 'a -> bool t
		(** The [=] operator. *)
  val (=%)   : ('a -> string t) -> string -> 'a -> bool t
		(** The [LIKE] operator. *)
  val (<>)   : ('a -> 'b t) -> ('a -> 'b t) -> 'a -> bool t
		(** The [<>] operator. *)
  val (<>%)  : ('a -> string t) -> string -> 'a -> bool t
		(** The [NOT LIKE] operator. *)
  val (>)    : ('a -> 'b t) -> ('a -> 'b t) -> 'a -> bool t
		(** The [>] operator. *)
  val (>=)   : ('a -> 'b t) -> ('a -> 'b t) -> 'a -> bool t
		(** The [>=] operator. *)
  val (<)    : ('a -> 'b t) -> ('a -> 'b t) -> 'a -> bool t
		(** The [<] operator. *)
  val (<=)   : ('a -> 'b t) -> ('a -> 'b t) -> 'a -> bool t
		(** The [<=] operator. *)
  val (&&)   : ('a -> bool t) -> ('a -> bool t) -> 'a -> bool t
		(** The [AND] operator. *)
  val (||)   : ('a -> bool t) -> ('a -> bool t) -> 'a -> bool t
		(** The [OR] operator. *)
  val (+)    : ('a -> int t) -> ('a -> int t) -> 'a -> int t
		(** The [+] operator for integers. *)
  val (-)    : ('a -> int t) -> ('a -> int t) -> 'a -> int t
		(** The [-] operator for integers. *)
  val ( * )  : ('a -> int t) -> ('a -> int t) -> 'a -> int t
		(** The [*] operator for integers. *)
  val (/)    : ('a -> int t) -> ('a -> int t) -> 'a -> int t
		(** The [/] operator for integers. *)
  val (+.)   : ('a -> float t) -> ('a -> float t) -> 'a -> float t
		(** The [+] operator for floats. *)
  val (-.)   : ('a -> float t) -> ('a -> float t) -> 'a -> float t
		(** The [-] operator for floats. *)
  val ( *. ) : ('a -> float t) -> ('a -> float t) -> 'a -> float t
		(** The [*] operator for floats. *)
  val (/.)   : ('a -> float t) -> ('a -> float t) -> 'a -> float t
		(** The [/] operator for floats. *)
  val (<<)   : ('a -> int t) -> int -> 'a -> int t
		(** The [<<] operator. *)
  val (>>)   : ('a -> int t) -> int -> 'a -> int t
		(** The [>>] operator. *)
  val (=?)   : ('a -> 'b t) -> ('a -> 'b t) list -> 'a -> bool t
		(** The [IN] operator. *)
  val (<>?)  : ('a -> 'b t) -> ('a -> 'b t) list -> 'a -> bool t
		(** The [NOT IN] operator. *)

  val is_null : ('a -> 'b option t) -> 'a -> bool t
		(** The [IS NULL] operator. *)
  val is_not_null : ('a -> 'b option t) -> 'a -> bool t
		(** The [IS NOT NULL] operator. *)

	(** {3 Creation of SQL values} *)

  val bool   : bool -> 'a -> bool t
		(** A boolean value expression. *)
  val int    : int -> 'a -> int t
		(** An int value expression. *)
  val float  : float -> 'a -> float t
		(** A float value expression. *)
  val string : string -> 'a -> string t
		(** A string value expression. *)
  val blob   : bytes -> 'a -> bytes t
		(** A blob value expression. *)

	(** {3 Casts} *)

  val as_bool   : ('a -> 'b t) -> 'a -> bool t
		(** Cast to bool. *)
  val as_int    : ('a -> 'b t) -> 'a -> int t
		(** Cast to int. *)
  val as_float  : ('a -> 'b t) -> 'a -> float t
		(** Cast to float. *)
  val as_string : ('a -> 'b t) -> 'a -> string t
		(** Cast to string. *)
  val as_blob   : ('a -> 'b t) -> 'a -> bytes t
		(** Cast to blob. *)

  (** Nullable SQL values. *)
  module Null : sig
    val bool   : bool -> 'a -> bool option t
			(** A nullable boolean value expression. *)
    val int    : int -> 'a -> int option t
			(** A nullable int value expression. *)
    val float  : float -> 'a -> float option t
			(** A nullable float value expression. *)
    val string : string -> 'a -> string option t
			(** A nullable string value expression. *)
    val blob   : bytes -> 'a -> bytes option t
			(** A nullable blob value expression. *)
  end
end

(** {2 Useful values for driver writers} *)

type handover =
  { expr : 'a. Common.build_step -> 'a t -> Common.build_step
  ; cast : 'b. 'b cast -> string
  }
  (** These are functions that allow an expression to be "handed over" to
      another function when they're being built. The [handover] type makes
			it possible to recursively build expressions that can contain
		  driver-specific constructors. *)

val build : placeholder:(int -> string)
         -> handover:handover
         -> Common.build_step -> 'a t
         -> Common.build_step
	(** Build an expression. *)

val build_function : placeholder:(int -> string)
                  -> handover:handover
                  -> Common.build_step
                  -> string
                  -> 'a t list
                  -> string
                  -> Common.build_step
	(** Build an expression that corresponds to an SQL function. *)

val string_of_cast : ?handover:('a cast -> string) -> 'a cast -> string
	(** Convert a [CAST] expression to a string representation. *)

type ('a, 'b) expr = 'b t

module Vector : Vector.S with type ('a, 'b) elem := ('a, 'b) expr
	(** A module for vectors of expressions. *)
