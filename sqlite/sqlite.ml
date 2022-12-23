(** Sequoia SQLite driver *)

open Printf
open Sequoia.Common

module D = struct let placeholder _ = "?" end
module M = Sequoia.Make (D)

include (M : module type of M
  with module Lit    := M.Lit
   and module Expr   := M.Expr
   and module Select := M.Select
   and module Update := M.Update
   and module Delete := M.Delete
   and module Field  := M.Field)

module Expr = struct
  include M.Expr
  include Base

  type 'a t +=
    (* Core functions *)
    | Abs : int t -> int t
    | Changes : int t
    | Char : int t list -> string t
		| Coalesce : 'a t list -> 'a t
    | Glob : string * string t -> bool t
    | Hex : bytes t -> string t
		| Ifnull : 'a t * 'a t -> 'a t
    | Instr : string t * string -> int t
    | Length : string t -> int t
    | Likelihood : bool t * float -> bool t
    | Likely : bool t -> bool t
    | Lower : string t -> string t
    | Ltrim : string t * string option -> string t
    | Max : 'a t list -> 'a t
    | Min : 'a t list -> 'a t
    | Nullif : 'a t * 'a t -> 'a t
    | Random : int t
    | Randomblob : int t -> bytes t
    | Replace : string t * string * string -> string t
    | Round : float t * int option -> float t
    | Rtrim : string t * string option -> string t
    | Substr : string t * int * int option -> string t
    | Trim : string t * string option -> string t
    | Typeof : 'a t -> string t
    | Unicode : string t -> int t
    | Unlikely : bool t -> bool t
    | Upper : string t -> string t
    | Zeroblob : int t -> bytes t
    (* Date and time functions *)
    | Date : string t * string list -> string t
    | Time : string t * string list -> string t
    | Julianday : string t * string list -> string t
    | Strftime : string * string t * string list -> string t
    (* Aggregate functions *)
    | Avg : float t -> float t
    | Count : 'a t option -> int t
    | Group_concat : string t * string option -> string t
    | MaxAgg : 'a t -> 'a t
    | MinAgg : 'a t -> 'a t
    | Sum : int t -> int t
    | Total : int t -> int t

  let changes () = fun _ -> Changes
  let char f = fun src -> Char (f src)
  let coalesce f = fun src -> Coalesce (f src)
  let glob pat f = fun src -> Glob (pat, f src)
  let hex f = fun src -> Hex (f src)
  let ifnull f g = fun src -> Ifnull (f src, g src)
  let instr f s = fun src -> Instr (f src, s)
  let length f = fun src -> Length (f src)
  let likelihood f p = fun src -> Likelihood (f src, p)
  let likely f = fun src -> Likely (f src)
  let lower f = fun src -> Lower (f src)
  let ltrim f ?chars = fun src -> Ltrim (f src, chars)
  let max f = fun src -> Max (f src)
  let min f = fun src -> Min (f src)
  let nullif f g = fun src -> Nullif (f src, g src)
  let random () = fun _ -> Random
  let randomblob f = fun src -> Randomblob (f src)
  let replace f s1 s2 = fun src -> Replace (f src, s1, s2)
  let round f ?digits = fun src -> Round (f src, digits)
  let rtrim f ?chars = fun src -> Rtrim (f src, chars)
  let substr f pos ?len = fun src -> Substr (f src, pos, len)
  let trim f ?chars = fun src -> Trim (f src, chars)
  let typeof f = fun src -> Typeof (f src)
  let unicode f = fun src -> Unicode (f src)
  let unlikely f = fun src -> Unlikely (f src)
  let upper f = fun src -> Upper (f src)
  let zeroblob f = fun src -> Zeroblob (f src)
  let date f l = fun src -> Date (f src, l)
  let time f l = fun src -> Time (f src, l)
  let julianday f l = fun src -> Julianday (f src, l)
  let strftime fmt f l = fun src -> Strftime (fmt, f src, l)
  let avg f = fun src -> Avg (f src)
  let count f = fun src -> Count (f src)
  let group_concat f ?sep = fun src -> Group_concat (f src, sep)
  let max_agg f = fun src -> MaxAgg (f src)
  let min_agg f = fun src -> MinAgg (f src)
  let sum f = fun src -> Sum (f src)
  let total f = fun src -> Total (f src)

  let build
    : type a. handover:handover -> build_step -> a t -> build_step =
    fun ~handover st e ->
      let fn ?(st = st) =
        M.Expr.build_function ~placeholder:D.placeholder ~handover st in
      match e with
      (* Core functions *)
      | Abs e -> fn "ABS(" [e] ")"
      | Changes -> fn "CHANGES(" [] ")"
      | Char l -> fn "CHAR(" l ")"
      | Coalesce l -> fn "COALESCE(" l ")"
      | Glob (pat, e) -> fn (sprintf "GLOB(%s, " pat) [e] ")"
      | Hex e -> fn "HEX(" [e] ")"
      | Ifnull (e1, e2) -> fn "IFNULL(" [e1; e2] ")"
      | Instr (e, s) -> fn "INSTR(" [e] (sprintf ", %s)" s)
      | Length e ->  fn "LENGTH(" [e] ")"
      | Likelihood (e, p) -> fn "LIKELYHOOD(" [e] (sprintf ", %f)" p)
      | Likely e -> fn "LIKELY(" [e] ")"
      | Lower e -> fn "LOWER(" [e] ")"
      | Ltrim (e, Some s) -> fn "LTRIM(" [e] (sprintf ", %s)" s)
      | Ltrim (e, None) -> fn "LTRIM(" [e] ")"
      | Max l -> fn "MAX(" l ")"
      | Min l -> fn "MIN(" l ")"
      | Nullif (e1, e2) -> fn "NULLIF(" [e1; e2] ")"
      | Random -> fn "RANDOM(" [] ")"
      | Randomblob e -> fn "RANDOMBLOB(" [e] ")"
      | Replace (e, s1, s2) -> fn "REPLACE(" [e] (sprintf ", %s, %s)" s1 s2)
      | Round (e, Some i) -> fn "ROUND(" [e] (sprintf ", %d)" i)
      | Round (e, None) -> fn "ROUND(" [e] ")"
      | Rtrim (e, Some s) -> fn "RTRIM(" [e] (sprintf ", %s)" s)
      | Rtrim (e, None) -> fn "RTRIM(" [e] ")"
      | Substr (e, pos, Some len) -> fn "SUBSTR(" [e] (sprintf ", %d, %d)" pos len)
      | Substr (e, pos, None) -> fn "SUBSTR(" [e] (sprintf ", %d)" pos)
      | Trim (e, Some s) -> fn "TRIM(" [e] (sprintf ", %s)" s)
      | Trim (e, None) -> fn "TRIM(" [e] ")"
      | Typeof e -> fn "TYPEOF(" [e] ")"
      | Unicode e -> fn "UNICODE(" [e] ")"
      | Unlikely e -> fn "UNLIKELY(" [e] ")"
      | Upper e -> fn "UPPER(" [e] ")"
      | Zeroblob e -> fn "ZEROBLOB(" [e] ")"
      (* Date and time functions *)
      | Date (e, l) -> fn "DATE(" [e] (sprintf "%s)" (String.concat ", " l))
      | Time (e, l) -> fn "TIME(" [e] (sprintf "%s)" (String.concat ", " l))
      | Julianday (e, l) ->
          fn "JULIANDAY(" [e] (sprintf "%s)" (String.concat ", " l))
      | Strftime (fmt, e, l) ->
          fn (sprintf "STRFTIME(%s, " fmt) [e]
             (sprintf "%s)" (String.concat ", " l))
      (* Aggregate functions *)
      | Avg e -> fn "AVG(" [e] ")"
      | Count None -> fn "COUNT(*" [] ")"
      | Count (Some e) -> fn "COUNT(" [e] ")"
      | Group_concat (e, Some sep) -> fn "GROUP_CONCAT(" [e] (sprintf ", %s)" sep)
      | Group_concat (e, None) -> fn "GROUP_CONCAT(" [e] ")"
      | MaxAgg e -> fn "MAX(" [e] ")"
      | MinAgg e -> fn "MIN(" [e] ")"
      | Sum e -> fn "SUM(" [e] ")"
      | Total e -> fn "TOTAL(" [e] ")"
      (* Handover to other expressions *)
      | e -> handover.expr st e
end

module Select = struct
  let expr_build st e = Expr.build st e
  let cast_handover = Expr.string_of_cast

  include M.Select

  let seal stmt =
    let rec expr_handover
      : type a. build_step -> a M.Expr.t -> build_step =
      fun st e ->
        let build st e =
          Expr.build
            ~handover:{ M.Expr.expr = expr_handover; cast = cast_handover }
            st
            e in
        expr_build
          ~handover:{ M.Expr.expr = build; cast = cast_handover } st e in
    seal ~handover:{ M.Expr.expr = expr_handover; cast = cast_handover } stmt
end

module Update = struct
  let expr_build st e = Expr.build st e
  let cast_handover = Expr.string_of_cast

  include M.Update

  let seal stmt =
    let rec expr_handover
      : type a. build_step -> a M.Expr.t -> build_step =
      fun st e ->
        let build st e =
          Expr.build
            ~placeholder:D.placeholder
            ~handover:{ M.Expr.expr = expr_handover; cast = cast_handover }
            st
            e in
        expr_build
          ~handover:{ M.Expr.expr = build; cast = cast_handover } st e in
    seal ~handover:{ M.Expr.expr = expr_handover; cast = cast_handover } stmt
end

module Delete = struct
  let expr_build st e = Expr.build st e
  let cast_handover = Expr.string_of_cast

  include M.Delete

  let seal stmt =
    let rec expr_handover
      : type a. build_step -> a M.Expr.t -> build_step =
      fun st e ->
        let build st e =
          Expr.build
            ~placeholder:D.placeholder
            ~handover:{ M.Expr.expr = expr_handover; cast = cast_handover }
            st
            e in
        expr_build
          ~handover:{ M.Expr.expr = build; cast = cast_handover } st e in
    seal ~handover:{ M.Expr.expr = expr_handover; cast = cast_handover } stmt
end
