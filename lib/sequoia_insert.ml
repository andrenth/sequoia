open Printf
open Sequoia_common
open Sequoia_query_common

module Driver = Sequoia_driver
module Field  = Sequoia_field
module Lit    = Sequoia_lit
module Table  = Sequoia_table
module Vector = Sequoia_vector
module Nat    = Vector.Nat

module type S = sig
  module Vector : Vector.S with type ('t, 'a) elem := ('t, 'a) Field.t

  type _ t

  val insert : into:'t Table.t
            -> fields:('t, 'a, 'n Nat.s) Vector.t
            -> values:('u, 'a, 'm Nat.s, 'n Nat.s) Lit.Vector.matrix
            -> 't t

  val seal : 't t -> string * Sequoia_param.t list
end

module Make (D : Driver.S) : S = struct
  module Vector = Vector.Make(struct
    type ('t, 'a) elem = ('t, 'a) Field.t
  end)

  type _ t =
    | I :
        { table  : 't Table.t
        ; fields : ('t, 'a, 'n Nat.s) Vector.t
        ; values : ('u, 'a, 'm Nat.s, 'n Nat.s) Lit.Vector.matrix
        } -> 't t

  let insert =
    fun ~into ~fields ~values ->
      I { table = into; fields; values }

  let rec join_fields
    : type t a n. (t, a, n) Vector.t -> string =
    fun flds ->
      let open Vector in
      match flds with
      | [] -> assert false
      | [f] -> Field.name f
      | f::fs -> Field.name f ^ ", " ^ join_fields fs

  let expr_placeholders i vs =
    let open Lit in
    let rec eps
      : type t a n. int -> (t, a, n) Lit.Vector.t -> string list =
      fun i exprs ->
        let open Lit.Vector in
        match exprs with
        | [] -> []
        | _::es -> D.placeholder i :: eps (i+1) es in
    eps i vs

  let placeholders
    : type a m. ('v, a, m, 'n) Lit.Vector.matrix -> string =
    fun values ->
      let rec pss
        : type a m. int -> ('v, a, m, 'n) Lit.Vector.matrix -> string =
        fun i vals ->
          let open Lit.Vector in
          match vals with
          | [] -> ""
          | [v] ->
              let ps = expr_placeholders i v in
              sprintf "(%s)" (String.concat ", " ps)
          | v::vs ->
              let ps = expr_placeholders i v in
              let n = i + List.length ps in
              sprintf "(%s)\n%s" (String.concat ", " ps) (pss n vs) in
      pss 1 values

  let params_of_values
    : type a. (a, 'b, 'm, 'n) Lit.Vector.matrix -> Param.t list =
    fun values ->
    Lit.Vector.matrix_fold_left
      { Lit.Vector.f = fun acc e -> Lit.to_param e :: acc }
      []
      values

  let seal (I { table; fields; values }) =
    let s =
      sprintf "INSERT INTO %s (%s) VALUES\n%s"
        (Table.to_string table)
        (join_fields fields)
        (placeholders values) in
    s, List.rev @@ params_of_values values
end
