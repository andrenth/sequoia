type 'a t = ..
type 'a cast = ..

module Base : sig
  type 'a cast +=
    | Bool : bool cast
    | Int : int cast
    | Float : float cast
    | String : string cast
    | Blob : bytes cast

  type 'a t +=
    | Lit : 'a Sequoia_lit.t -> 'a t
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

  val (=)    : ('a -> 'b t) -> ('a -> 'b t) -> 'a -> bool t
  val (=%)   : ('a -> string t) -> string -> 'a -> bool t
  val (<>)   : ('a -> 'b t) -> ('a -> 'b t) -> 'a -> bool t
  val (<>%)  : ('a -> string t) -> string -> 'a -> bool t
  val (>)    : ('a -> 'b t) -> ('a -> 'b t) -> 'a -> bool t
  val (>=)   : ('a -> 'b t) -> ('a -> 'b t) -> 'a -> bool t
  val (<)    : ('a -> 'b t) -> ('a -> 'b t) -> 'a -> bool t
  val (<=)   : ('a -> 'b t) -> ('a -> 'b t) -> 'a -> bool t
  val (&&)   : ('a -> bool t) -> ('a -> bool t) -> 'a -> bool t
  val (||)   : ('a -> bool t) -> ('a -> bool t) -> 'a -> bool t
  val (+)    : ('a -> int t) -> ('a -> int t) -> 'a -> int t
  val (-)    : ('a -> int t) -> ('a -> int t) -> 'a -> int t
  val ( * )  : ('a -> int t) -> ('a -> int t) -> 'a -> int t
  val (/)    : ('a -> int t) -> ('a -> int t) -> 'a -> int t
  val (+.)   : ('a -> float t) -> ('a -> float t) -> 'a -> float t
  val (-.)   : ('a -> float t) -> ('a -> float t) -> 'a -> float t
  val ( *. ) : ('a -> float t) -> ('a -> float t) -> 'a -> float t
  val (/.)   : ('a -> float t) -> ('a -> float t) -> 'a -> float t
  val (<<)   : ('a -> int t) -> int -> 'a -> int t
  val (>>)   : ('a -> int t) -> int -> 'a -> int t
  val (=?)   : ('a -> 'b t) -> ('a -> 'b t) list -> 'a -> bool t
  val (<>?)  : ('a -> 'b t) -> ('a -> 'b t) list -> 'a -> bool t

  val is_null : ('a -> 'b option t) -> 'a -> bool t
  val is_not_null : ('a -> 'b option t) -> 'a -> bool t

  val bool   : bool -> 'a -> bool t
  val int    : int -> 'a -> int t
  val float  : float -> 'a -> float t
  val string : string -> 'a -> string t
  val blob   : bytes -> 'a -> bytes t

  val as_bool   : ('a -> 'b t) -> 'a -> bool t
  val as_int    : ('a -> 'b t) -> 'a -> int t
  val as_float  : ('a -> 'b t) -> 'a -> float t
  val as_string : ('a -> 'b t) -> 'a -> string t
  val as_blob   : ('a -> 'b t) -> 'a -> bytes t

  module Null : sig
    val bool   : bool -> 'a -> bool option t
    val int    : int -> 'a -> int option t
    val float  : float -> 'a -> float option t
    val string : string -> 'a -> string option t
    val blob   : bytes -> 'a -> bytes option t
  end
end

type handover =
  { expr : 'a. Sequoia_common.build_step -> 'a t -> Sequoia_common.build_step
  ; cast : 'b. 'b cast -> string
  }

val build : placeholder:(int -> string)
         -> handover:handover
         -> Sequoia_common.build_step -> 'a t
         -> Sequoia_common.build_step

val build_function : placeholder:(int -> string)
                  -> handover:handover
                  -> Sequoia_common.build_step
                  -> string
                  -> 'a t list
                  -> string
                  -> Sequoia_common.build_step

val string_of_cast : ?handover:('a cast -> string) -> 'a cast -> string

type ('a, 'b) expr = 'b t

module Vector : Sequoia_vector.S with type ('a, 'b) elem := ('a, 'b) expr
