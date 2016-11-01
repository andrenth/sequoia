open Printf

module D = struct let placeholder _ = "?" end
module M = Sequoia.Make (D)

include (M : module type of M
  with module Expr   := M.Expr
   and module Select := M.Select
   and module Field  := M.Field)

module Expr = struct
  include M.Expr

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
end

module Select = struct
  include (M.Select : module type of M.Select
    with module Expr := M.Select.Expr)

  module Expr = struct
    include M.Select.Expr

    let rec builder
      : type a. build_step -> a t -> build_step =
      fun st e ->
        let handover = { handover = builder } in
        let fn ?(st = st) = build_function ~handover st in
        let open Expr in
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
        (* Handover to basic expressions *)
        | e -> build ~handover st e

    let avg f = fun src -> Expr.Avg (f src)
    let changes () = fun src -> Expr.Changes
    let char f = fun src -> Expr.Char (f src)
    let coalesce f = fun src -> Expr.Coalesce (f src)
    let glob pat f = fun src -> Expr.Glob (pat, f src)
    let hex f = fun src -> Expr.Hex (f src)
    let ifnull f g = fun src -> Expr.Ifnull (f src, g src)
    let instr f s = fun src -> Expr.Instr (f src, s)
    let length f = fun src -> Expr.Length (f src)
    let likelihood f p = fun src -> Expr.Likelihood (f src, p)
    let likely f = fun src -> Expr.Likely (f src)
    let lower f = fun src -> Expr.Lower (f src)
    let ltrim f ?chars = fun src -> Expr.Ltrim (f src, chars)
    let max f = fun src -> Expr.Max (f src)
    let min f = fun src -> Expr.Min (f src)
    let nullif f g = fun src -> Expr.Nullif (f src, g src)
    let random () = fun src -> Expr.Random
    let randomblob f = fun src -> Expr.Randomblob (f src)
    let replace f s1 s2 = fun src -> Expr.Replace (f src, s1, s2)
    let round f ?digits = fun src -> Expr.Round (f src, digits)
    let rtrim f ?chars = fun src -> Expr.Rtrim (f src, chars)
    let substr f pos ?len = fun src -> Expr.Substr (f src, pos, len)
    let trim f ?chars = fun src -> Expr.Trim (f src, chars)
    let typeof f = fun src -> Expr.Typeof (f src)
    let unicode f = fun src -> Expr.Unicode (f src)
    let unlikely f = fun src -> Expr.Unlikely (f src)
    let upper f = fun src -> Expr.Upper (f src)
    let zeroblob f = fun src -> Expr.Zeroblob (f src)
    let date f l = fun src -> Expr.Date (f src, l)
    let time f l = fun src -> Expr.Time (f src, l)
    let julianday f l = fun src -> Expr.Julianday (f src, l)
    let strftime fmt f l = fun src -> Expr.Strftime (fmt, f src, l)
    let avg f = fun src -> Expr.Avg (f src)
    let count f = fun src -> Expr.Count (f src)
    let group_concat f ?sep = fun src -> Expr.Group_concat (f src, sep)
    let max_agg f = fun src -> Expr.MaxAgg (f src)
    let min_agg f = fun src -> Expr.MinAgg (f src)
    let sum f = fun src -> Expr.Sum (f src)
    let total f = fun src -> Expr.Total (f src)
  end

  let seal stmt = seal ~handover:{ Expr.handover = Expr.builder } stmt
end
