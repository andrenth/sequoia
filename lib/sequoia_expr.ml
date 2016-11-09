open Printf
open Sequoia_common

module Driver = Sequoia_driver
module Lit    = Sequoia_lit
module Param  = Sequoia_param

type 'a t = ..

module Base = struct
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

  let (=)    f g = fun x -> Eq (f x, g x)
  let (=%)   f s = fun x -> Like (f x, s)
  let (<>)   f g = fun x -> Neq (f x, g x)
  let (<>%)  f s = fun x -> Not_like (f x, s)
  let (>)    f g = fun x -> Gt (f x, g x)
  let (>=)   f g = fun x -> Ge (f x, g x)
  let (<)    f g = fun x -> Lt (f x, g x)
  let (<=)   f g = fun x -> Le (f x, g x)
  let (&&)   f g = fun x -> And (f x, g x)
  let (||)   f g = fun x -> And (f x, g x)
  let (+)    f g = fun x -> IAdd (f x, g x)
  let (-)    f g = fun x -> ISub (f x, g x)
  let ( * )  f g = fun x -> IMul (f x, g x)
  let (/)    f g = fun x -> IDiv (f x, g x)
  let (+.)   f g = fun x -> FAdd (f x, g x)
  let (-.)   f g = fun x -> FSub (f x, g x)
  let ( *. ) f g = fun x -> FMul (f x, g x)
  let (/.)   f g = fun x -> FDiv (f x, g x)
  let (<<)   f i = fun x -> LShift (f x, i)
  let (>>)   f i = fun x -> RShift (f x, i)
  let (=?)   f l = fun x -> In (f x, List.map (fun f -> f x) l)
  let (<>?)  f l = fun x -> Not_in (f x, List.map (fun f -> f x) l)

  let is_null     f = fun x -> Is_null (f x)
  let is_not_null f = fun x -> Is_not_null (f x)

  let bool b = fun _ -> Lit (Lit.bool b)
  let int i = fun _ -> Lit (Lit.int i)
  let float x = fun _ -> Lit (Lit.float x)
  let string s = fun _ -> Lit (Lit.string s)
  let blob b = fun _ -> Lit (Lit.blob b)

  module Null = struct
    let bool b = fun _ -> Lit (Lit.Null.bool b)
    let int i = fun _ -> Lit (Lit.Null.int i)
    let float x = fun _ -> Lit (Lit.Null.float x)
    let string s = fun _ -> Lit (Lit.Null.string s)
    let blob b = fun _ -> Lit (Lit.Null.blob b)
  end
end

type handover =
  { handover : 'a. build_step -> 'a t -> build_step }

let rec build
  : type a. placeholder:(int -> string)
         -> handover:handover
         -> build_step
         -> a t
         -> build_step =
  fun ~placeholder ~handover st e ->
    let build_lit : type a. a Lit.t -> build_step = fun lit ->
      Lit.build ~placeholder st lit in
    let build_binop : type a b. string -> a t -> b t -> build_step =
      fun op e1 e2 ->
        build_binop ~placeholder ~handover st op e1 e2 in
    let build_function
      : type a. build_step -> string -> a t list -> string -> build_step =
      fun st pref l suff ->
        build_function ~placeholder ~handover st pref l suff in
    match e with
    | Base.Lit ((Lit.Bool _) as lit) -> build_lit lit
    | Base.Lit ((Lit.Int _) as lit) -> build_lit lit
    | Base.Lit ((Lit.Float _) as lit) -> build_lit lit
    | Base.Lit ((Lit.String _) as lit) -> build_lit lit
    | Base.Lit ((Lit.Blob _) as lit) -> build_lit lit
    | Base.Lit ((Lit.Null.Bool _) as lit) -> build_lit lit
    | Base.Lit ((Lit.Null.Int _) as lit) -> build_lit lit
    | Base.Lit ((Lit.Null.Float _) as lit) -> build_lit lit
    | Base.Lit ((Lit.Null.String _) as lit) -> build_lit lit
    | Base.Lit ((Lit.Null.Blob _) as lit) -> build_lit lit
    | Base.Eq (e1, e2) -> build_binop "=" e1 e2
    | Base.Neq (e1, e2) -> build_binop "<>" e1 e2
    | Base.Gt (e1, e2) -> build_binop ">" e1 e2
    | Base.Ge (e1, e2) -> build_binop ">=" e1 e2
    | Base.Lt (e1, e2) -> build_binop "<" e1 e2
    | Base.Le (e1, e2) -> build_binop "<=" e1 e2
    | Base.And (e1, e2) -> build_binop "AND" e1 e2
    | Base.Or (e1, e2) -> build_binop "OR" e1 e2
    | Base.IAdd (e1, e2) -> build_binop "+" e1 e2
    | Base.ISub (e1, e2) -> build_binop "-" e1 e2
    | Base.IMul (e1, e2) -> build_binop "*" e1 e2
    | Base.IDiv (e1, e2) -> build_binop "/" e1 e2
    | Base.FAdd (e1, e2) -> build_binop "+" e1 e2
    | Base.FSub (e1, e2) -> build_binop "-" e1 e2
    | Base.FMul (e1, e2) -> build_binop "*" e1 e2
    | Base.FDiv (e1, e2) -> build_binop "/" e1 e2
    | Base.LShift (e, i) ->
        let st = build ~placeholder ~handover st e in
        { repr = sprintf "(%s) << %s" st.repr (placeholder st.pos)
        ; params = st.params @ [Param.Int i]
        ; pos = st.pos + 1
        }
    | Base.RShift (e, i) ->
        let st = build ~placeholder ~handover st e in
        { repr = sprintf "(%s) >> %s" st.repr (placeholder st.pos)
        ; params = st.params @ [Param.Int i]
        ; pos = st.pos + 1
        }
    | Base.Like (e, pat) ->
        let st = build ~placeholder ~handover st e in
        { repr = sprintf "(%s) LIKE %s" st.repr (placeholder st.pos)
        ; params = st.params @ [Param.String pat]
        ; pos = st.pos + 1
        }
    | Base.Not_like (e, pat) ->
        let st = build ~placeholder ~handover st e in
        { repr = sprintf "(%s) NOT LIKE %s" st.repr (placeholder st.pos)
        ; params = st.params @ [Param.String pat]
        ; pos = st.pos + 1
        }
    | Base.In (e, l) ->
        let st1 = build ~placeholder ~handover st e in
        let st2 = build_function st1 (st1.repr ^ " IN(") l ")" in
        { st2 with params = st1.params @ st2.params }
    | Base.Not_in (e, l) ->
        let st1 = build ~placeholder ~handover st e in
        let st2 = build_function st1 (st1.repr ^ " NOT IN(") l ")" in
        { st2 with params = st1.params @ st2.params }
    | Base.Is_not_null e ->
        build_function st "" [e] " IS NOT NULL"
    | Base.Is_null e ->
        build_function st "" [e] " IS NULL"
    | e ->
        handover.handover st e

and build_binop
  : type a b. placeholder:(int -> string) -> handover:handover -> build_step
           -> string -> a t -> b t -> build_step =
  fun ~placeholder ~handover st op e1 e2 ->
    let st1 = build ~placeholder ~handover st e1 in
    let st2 = build ~placeholder ~handover st1 e2 in
    { repr = sprintf "(%s) %s (%s)" st1.repr op st2.repr
    ; params = st1.params @ st2.params
    ; pos = st2.pos
    }

and build_function
  : type a. placeholder:(int -> string)
         -> handover:handover
         -> build_step
         -> string
         -> a t list
         -> string
         -> build_step =
  fun ~placeholder ~handover st left l right ->
    let st = build_arg_list ~placeholder ~handover st l in
    { st with repr = left ^ st.repr ^ right }

and build_arg_list
  : type a. placeholder:(int -> string)
         -> handover:handover
         -> build_step
         -> a t list
         -> build_step =
  fun ~placeholder ~handover st l ->
    fst @@ List.fold_left
      (fun (acc, i) e ->
        let st = build ~placeholder ~handover st e in
        let repr =
          if i = 0 then begin
            st.repr
          end else
            sprintf "%s, %s" acc.repr st.repr in
        let params = acc.params @ st.params in
        ({ repr; params; pos = st.pos }, i + 1))
      ({ blank_step with pos = st.pos }, 0)
      l

type ('a, 'b) expr = 'b t

module Vector = Sequoia_vector.Make(struct
  type ('a, 'b) elem = ('a, 'b) expr
end)
