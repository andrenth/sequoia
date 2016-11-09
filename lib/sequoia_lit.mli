type 'a t = ..

type 'a t +=
  | Bool : bool -> bool t
  | Int : int -> int t
  | Float : float -> float t
  | String : string -> string t
  | Blob : bytes -> bytes t

val bool : bool -> bool t
val int : int -> int t
val float : float -> float t
val string : string -> string t
val blob : bytes -> bytes t

module Null : sig
  type 'a t +=
    | Bool : bool -> bool option t
    | Int : int -> int option t
    | Float : float -> float option t
    | String : string -> string option t
    | Blob : bytes -> bytes option t

  val bool : bool -> bool option t
  val int : int -> int option t
  val float : float -> float option t
  val string : string -> string option t
  val blob : bytes -> bytes option t
end

val build : placeholder:(int -> string) -> Sequoia_common.build_step -> 'a t
         -> Sequoia_common.build_step
val to_param : 'a t -> Sequoia_param.t

type ('a, 'b) lit = 'b t

module Vector : Sequoia_vector.S with type ('a, 'b) elem := ('a, 'b) lit
