(** SQL literal values. *)

type 'a t = ..
  (** The type of literals. Can be extended by drivers. *)

type 'a t +=
  | Bool : bool -> bool t
  | Int : int -> int t
  | Float : float -> float t
  | String : string -> string t
  | Blob : bytes -> bytes t
  (** Basic SQL literals. *)

(** {3 Literal creation}

    The functions below create literal values of the appropriate types. *)

val bool : bool -> bool t
val int : int -> int t
val float : float -> float t
val string : string -> string t
val blob : bytes -> bytes t

(** Literals for nullable fields. *)
module Null : sig
  type 'a t +=
    | Bool : bool -> bool option t
    | Int : int -> int option t
    | Float : float -> float option t
    | String : string -> string option t
    | Blob : bytes -> bytes option t
    (** Basic SQL literals for nullable fields. *)

  (** {3 Literal creation}

      The functions below create literal values of the appropriate types. *)

  val bool : bool -> bool option t
  val int : int -> int option t
  val float : float -> float option t
  val string : string -> string option t
  val blob : bytes -> bytes option t
end

type ('a, 'b) lit = 'b t

module Vector : Sequoia_vector.S with type ('a, 'b) elem := ('a, 'b) lit
  (** Vectors of SQL literals. *)

(** {2 Functions useful for driver writers} *)

val build : placeholder:(int -> string) -> Sequoia_common.build_step -> 'a t
         -> Sequoia_common.build_step
  (** Build a literal value. *)

val to_param : 'a t -> Sequoia_param.t
  (** Convert a literal value to a query parameter. *)
