open Sequoia_common

module Driver = Sequoia_driver
module Param  = Sequoia_param

type 'a t = ..

type 'a t +=
  | Bool : bool -> bool t
  | Int : int -> int t
  | Float : float -> float t
  | String : string -> string t
  | Blob : bytes -> bytes t

let bool b = Bool b
let int i = Int i
let float x = Float x
let string s = String s
let blob b = Blob b

module Null = struct
  type 'a t +=
    | Bool : bool -> bool option t
    | Int : int -> int option t
    | Float : float -> float option t
    | String : string -> string option t
    | Blob : bytes -> bytes option t

  let bool b = Bool b
  let int i = Int i
  let float x = Float x
  let string s = String s
  let blob b = Blob b
end

let build :
  type a. placeholder:(int -> string)
       -> build_step
       -> a t
       -> build_step =
  fun ~placeholder st lit ->
    let build_param = build_param placeholder st in
    match lit with
    | Bool b -> build_param (Param.Bool b)
    | Int i -> build_param (Param.Int i)
    | Float x -> build_param (Param.Float x)
    | String s -> build_param (Param.String s)
    | Blob b -> build_param (Param.Blob b)
    | Null.Bool b -> build_param (Param.Bool b)
    | Null.Int i -> build_param (Param.Int i)
    | Null.Float x -> build_param (Param.Float x)
    | Null.String s -> build_param (Param.String s)
    | Null.Blob b -> build_param (Param.Blob b)
    | _ -> assert false

let to_param : type a. a t -> Param.t = function
  | Bool b -> Param.Bool b
  | Int i -> Param.Int i
  | Float x -> Param.Float x
  | String s -> Param.String s
  | Blob b -> Param.Blob b
  | Null.Bool b -> Param.Bool b
  | Null.Int i -> Param.Int i
  | Null.Float x -> Param.Float x
  | Null.String s -> Param.String s
  | Null.Blob b -> Param.Blob b
  | _ -> assert false

type ('a, 'b) lit = 'b t

module Vector = Sequoia_vector.Make(struct
  type ('a, 'b) elem = ('a, 'b) lit
end)
