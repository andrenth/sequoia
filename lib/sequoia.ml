open Printf

module type NAMED = sig
  type t
  val name : string
end

module type Driver = sig
  val placeholder : int -> string
end

module Make (D : Driver) = struct
  module Param = struct
    type t = ..
    type t +=
      | Bool of bool
      | Int of int
      | Float of float
      | String of string
      | Blob of bytes
  end

  type build_step =
    { repr : string
    ; params : Param.t list
    ; pos : int
    }

  let blank_step = { repr = ""; params = []; pos = 0 }

  module Table = struct
    type 'a t = { name : string }
    let called name = { name }
    let to_string { name } = name
  end

  module Field = struct
    type ('t, 'a) t = ..
    type ('t, 'a) t +=
      | Bool : string * 't Table.t -> ('t, bool) t
      | Int : string * 't Table.t -> ('t, int) t
      | Float : string * 't Table.t -> ('t, float) t
      | String : string * 't Table.t -> ('t, string) t
      | Blob : string * 't Table.t -> ('t, bytes) t

    type ('t1, 't2) foreign_key = ('t1, int) t * ('t2, int) t

    let table: type u a. (u, a) t -> u Table.t = function
      | Bool (_, t) -> t
      | Int (_, t) -> t
      | Float (_, t) -> t
      | String (_, t) -> t
      | Blob (_, t) -> t

    let to_string : type a b. (a, b) t -> string = function
      | Bool (name, table) -> sprintf "%s.%s" table.Table.name name
      | Int (name, table) -> sprintf "%s.%s" table.Table.name name
      | Float (name, table) -> sprintf "%s.%s" table.Table.name name
      | String (name, table) -> sprintf "%s.%s" table.Table.name name
      | Blob (name, table) -> sprintf "%s.%s" table.Table.name name

    let foreign_key table name ~references = (Int (name, table), references)
    let bool table name = Bool (name, table)
    let int table name = Int (name, table)
    let float table name = Float (name, table)
    let string table name = String (name, table)
    let blob table name = Blob (name, table)
  end

  module type TABLE = sig
    type t
    val table : t Table.t
    module Field : sig
      type table = t
      type 'a t = (table, 'a) Field.t
      type 't foreign_key = (table, 't) Field.foreign_key
      val int : string -> int t
      val string : string -> string t
      val foreign_key : string -> references:('t, int) Field.t -> 't foreign_key
    end
  end

  module MakeTable (T: NAMED) : TABLE = struct
    type t = T.t
    let table = Table.called T.name
    module Field = struct
      type table = t
      type 'a t = (table, 'a) Field.t
      type 't foreign_key = (table, 't) Field.foreign_key
      let int = Field.int table
      let string = Field.string table
      let foreign_key name ~references = Field.foreign_key table name ~references
    end
  end

  let table name =
    (module struct
      include MakeTable (struct type t let name = name end)
    end : TABLE)

  module rec Select : sig
    type _ t

    type 's source

    type ('s1, 't1, 't2, 's2) join_steps =
      | There : (('t2 -> _) as 's1, 't1, 't2, 't1 -> 's1) join_steps
      | Skip : ('s1, 't1, 't2, 's2) join_steps -> ('a -> 's1, 't1, 't2, 'a -> 's2) join_steps

    type ('s1, 't1, 't2, 's2) steps =
      | There : (('t2 -> _) as 's1, 't1, 't2, 's1) steps
      | Skip : ('s1, 't1, 't2, 's2) steps -> ('a -> 's1, 't1, 't2, 'a -> 's2) steps

    val seal : ?handover:Expr.handover -> 's t -> string * Param.t list
    val select : ('a, 's) Expr.listmk -> 's source -> 's t

    val from : 't Table.t -> ('t -> unit) source
    val left_join  : ('a -> ('t1, 't2) Field.foreign_key)
                  -> 'a
                  -> ('s1, 't1, 't2, 's2) join_steps
                  -> 's1 source
                  -> 's2 source
    val right_join : ('a -> ('t1, 't2) Field.foreign_key)
                  -> 'a
                  -> ('s1, 't1, 't2, 's2) join_steps
                  -> 's1 source
                  -> 's2 source
    val inner_join : ('a -> ('t1, 't2) Field.foreign_key)
                  -> 'a
                  -> ('s1, 't1, 't2, 's2) join_steps
                  -> 's1 source
                  -> 's2 source

    val having_one : ('t1, 't2) Field.foreign_key -> ('t1, 't2) Field.foreign_key
    val belonging_to : ('t1, 't2) Field.foreign_key -> ('t2, 't1) Field.foreign_key

    val where : ('a source -> 'b Expr.t) -> 'a t -> 'a t
    val group_by : ('s source -> 'a Expr.t) -> 's t -> 's t
    val order_by : ('a, 's) Expr.listmk -> 's t -> 's t
    val limit : ?offset:int -> int -> 'a t -> 'a t

  end = struct
    type ('s1, 't1, 't2, 's2) join_steps =
      | There : (('t2 -> _) as 's1, 't1, 't2, 't1 -> 's1) join_steps
      | Skip : ('s1, 't1, 't2, 's2) join_steps -> ('a -> 's1, 't1, 't2, 'a -> 's2) join_steps

    type join
      = Left
      | Right
      | Inner

    type 's source =
      | From : 't Table.t -> ('t -> unit) source
      | Join : join * ('t1, 't2) Field.foreign_key * 's1 source * ('s1, 't1, 't2, 's2) join_steps -> 's2 source

    type ('s1, 't1, 't2, 's2) steps =
      | There : (('t2 -> _) as 's1, 't1, 't2, 's1) steps
      | Skip : ('s1, 't1, 't2, 's2) steps -> ('a -> 's1, 't1, 't2, 'a -> 's2) steps

    type _ t =
      | S :
          { source   : 's source
          ; select   : 'a Expr.list
          ; where    : 'b Expr.t option
          ; group_by : 'c Expr.t option
          ; order_by : 'd Expr.list
          ; limit    : (int * int) option
          } -> 's t

    let join_to_string = function
      | Left -> "LEFT"
      | Right -> "RIGHT"
      | Inner -> "INNER"

    let join_exprs ~handover st =
      Expr.fold_left
        { Expr.f = fun (st, i) e ->
          let st' = Expr.build ~handover st e in
          if i = 0 then
            (st', 1)
          else
            let st =
              { repr = st.repr ^ ", " ^ st'.repr
              ; params = st.params @ st'.params
              ; pos = st'.pos
              } in
            (st, i + 1) }
        ({ blank_step with pos = st.pos }, 0)

    let build_select ~handover st exprs =
      fst (join_exprs ~handover st exprs)

    let rec build_source
      : type s. handover:Expr.handover -> build_step -> 'a Expr.list -> s source
             -> build_step =
      fun ~handover st sel -> function
        | From t ->
            let st = build_select ~handover st sel in
            { repr = sprintf "SELECT %s\nFROM %s" st.repr (Table.to_string t)
            ; params = st.params
            ; pos = st.pos
            }
        | Join (join, (a, b), src, _) ->
            let st = build_source ~handover st sel src in
            let repr =
              st.repr
                ^ "\n"
                ^ sprintf "%s JOIN %s ON %s = %s"
                    (join_to_string join)
                    (Table.to_string (Field.table a))
                    (Field.to_string a)
                    (Field.to_string b) in
            { repr
            ; params = st.params
            ; pos = st.pos
            }

    let build_where
      : type a. handover:Expr.handover -> build_step -> a Expr.t option
             -> build_step =
      fun ~handover st -> function
        | Some expr ->
            let st = Expr.build ~handover st expr in
            { st with repr = sprintf "WHERE (%s)" st.repr }
        | None ->
            { blank_step with pos = st.pos }

    let build_group_by
      : type a. handover:Expr.handover -> build_step -> a Expr.t option
             -> build_step =
      fun ~handover st -> function
        | Some expr ->
            let st = Expr.build ~handover st expr in
            { st with repr = sprintf "GROUP BY (%s)" st.repr }
        | None ->
            { blank_step with pos = st.pos }

    let build_order_by ~handover st (exprs:'a Expr.list) =
      let open Expr in
      match exprs with
      | [] -> st
      | es ->
          let st = fst (join_exprs ~handover st es) in
          { st with repr = "ORDER BY " ^ st.repr }

    let build_limit = fun st lim ->
      match lim with
      | Some (0, lim) ->
          { repr = sprintf "LIMIT %s" (D.placeholder st.pos)
          ; params = [Param.Int lim]
          ; pos = st.pos + 1
          }
      | Some (off, lim) ->
          { repr = sprintf "LIMIT %s, %s" (D.placeholder st.pos)  (D.placeholder (st.pos + 1))
          ; params = [Param.Int off; Param.Int lim]
          ; pos = st.pos + 2
          }
      | None ->
          { blank_step with pos = st.pos }

    let join_lines =
      List.fold_left
        (fun acc s ->
          if s = "" then
            acc
          else if acc = "" then
            s
          else
            acc ^ "\n" ^ s)
        ""

    let unknown = { Expr.handover = fun _ -> failwith "unknown expression" }

    let seal
      : type s. ?handover:Expr.handover -> s Select.t -> string * Param.t list =
      fun ?(handover = unknown) (S stmt) ->
        let st = { blank_step with pos = 1 } in
        let src_st = build_source ~handover st stmt.select stmt.source in
        let where_st = build_where ~handover src_st stmt.where in
        let group_by_st = build_group_by ~handover where_st stmt.group_by in
        let order_by_st = build_order_by ~handover group_by_st stmt.order_by in
        let limit_st = build_limit order_by_st stmt.limit in
        let params = src_st.params
                   @ where_st.params
                   @ group_by_st.params
                   @ order_by_st.params
                   @ limit_st.params in
        let repr =
          join_lines
          [ src_st.repr
          ; where_st.repr
          ; group_by_st.repr
          ; order_by_st.repr
          ; limit_st.repr
        ] in
        repr, params

    let to_string stmt = fst (seal stmt)

    let select : type s. ('a, s) Expr.listmk -> s source -> s Select.t = fun bl src ->
      S
        { source = src
        ; select = Expr.listmk_to_list src bl
        ; where = None
        ; group_by = None
        ; order_by = Expr.[]
        ; limit = None
        }

    let from t =
      From t

    let join kind f rel steps src =
      Join (kind, f rel, src, steps)

    let left_join  f rel steps src = join Left  f rel steps src
    let right_join f rel steps src = join Right f rel steps src
    let inner_join f rel steps src = join Inner f rel steps src

    let having_one rel = rel
    let belonging_to (fk, pk) = (pk, fk)

    let where expr (S stmt) =
      S { stmt with where = Some (expr stmt.source) }

    let group_by expr (S stmt) =
      S { stmt with group_by = Some (expr stmt.source) }

    let order_by : type s. ('a, s) Expr.listmk -> s Select.t -> s Select.t = fun bl (S stmt) ->
      S { stmt with order_by = Expr.listmk_to_list stmt.source bl }

    let limit ?(offset = 0) n (S stmt) =
      S { stmt with limit = Some (offset, n) }
  end

  and Expr : sig
    type 'a t = ..
    type 'a t +=
      | Bool : bool -> bool t
      | Int : int -> int t
      | Float : float -> float t
      | String : string -> string t
      | Blob : bytes -> bytes t
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
      | Is_not_null : 'a t -> bool t
      | Is_null : 'a t -> bool t
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
      | Field : ('t, 'a) Field.t * 's1 Select.source * ('s1, 't1, 't, 's2) Select.steps -> 'a t
      | Foreign : ('t, 't2) Field.foreign_key * 's1 Select.source * ('s1, 't1, 't, 's2) Select.steps -> 'a t
      | Select : 's Select.t -> 'a t

    type handover = { handover : 'a. build_step -> 'a t -> build_step }

    val build : handover:handover -> build_step -> 'a t -> build_step
    val build_param : build_step -> Param.t -> build_step
    val build_function : handover:handover -> build_step
                      -> string -> 'a t list -> string
                      -> build_step

    val bool : bool -> 's Select.source -> bool t
    val int : int -> 's Select.source -> int t
    val float : float -> 's Select.source -> float t
    val string : string -> 's Select.source -> string t
    val blob : string -> 's Select.source -> bytes t
    val field : ('t, 'a) Field.t -> ('b, 'c, 't, 'd) Select.steps -> 'b Select.source -> 'a t
    val foreign_key : ('t1, 't2) Field.foreign_key -> ('a, 'b, 't1, 'c) Select.steps -> 'a Select.source -> 'd t
    val subquery : 's Select.t -> 't Select.source -> 'c t

    val (=) : ('s Select.source -> 'a t) -> ('s Select.source -> 'a t) -> 's Select.source
           -> bool t
    val (<>) : ('s Select.source -> 'a t) -> ('s Select.source -> 'a t) -> 's Select.source
            -> bool t
    val (=%) : ('s Select.source -> string t) -> string -> 's Select.source -> bool t
    val (<>%) : ('s Select.source -> string t) -> string -> 's Select.source -> bool t
    val (>) : ('s Select.source -> 'a t) -> ('s Select.source -> 'a t) -> 's Select.source
           -> bool t
    val (>=) : ('s Select.source -> 'a t) -> ('s Select.source -> 'a t) -> 's Select.source
            -> bool t
    val (<) : ('s Select.source -> 'a t) -> ('s Select.source -> 'a t) -> 's Select.source
           -> bool t
    val (<=) : ('s Select.source -> 'a t) -> ('s Select.source -> 'a t) -> 's Select.source
            -> bool t
    val (&&) : ('s Select.source -> bool t) -> ('s Select.source -> bool t)
            -> 's Select.source -> bool t
    val (||) : ('s Select.source -> bool t) -> ('s Select.source -> bool t)
            -> 's Select.source -> bool t
    val (+) : ('s Select.source -> int t) -> ('s Select.source -> int t) -> 's Select.source
           -> int t
    val (-) : ('s Select.source -> int t) -> ('s Select.source -> int t) -> 's Select.source
           -> int t
    val ( * ) : ('s Select.source -> int t) -> ('s Select.source -> int t)
             -> 's Select.source -> int t
    val (/) : ('s Select.source -> int t) -> ('s Select.source -> int t) -> 's Select.source
           -> int t
    val (+.) : ('s Select.source -> float t) -> ('s Select.source -> float t)
            -> 's Select.source -> float t
    val (-.) : ('s Select.source -> float t) -> ('s Select.source -> float t)
            -> 's Select.source -> float t
    val ( *. ) : ('s Select.source -> float t) -> ('s Select.source -> float t)
              -> 's Select.source -> float t
    val (/.) : ('s Select.source -> float t) -> ('s Select.source -> float t)
            -> 's Select.source -> float t
    val (<<) : ('s Select.source -> int t) -> int -> 's Select.source -> int t
    val (>>) : ('s Select.source -> int t) -> int -> 's Select.source -> int t
    val (=?) : ('s Select.source -> 'a t) -> ('s Select.source -> 'a t) list
            -> 's Select.source -> bool t
    val (<>?) : ('s Select.source -> 'a t) -> ('s Select.source -> 'a t) list
             -> 's Select.source -> bool t
    val is_null : ('s Select.source -> 'a t) -> 's Select.source -> bool t
    val is_not_null : ('s Select.source -> 'a t) -> 's Select.source -> bool t

    type _ list =
      | [] : 'a list
      | (::) : 'a t * 'b list -> ('a * 'b) list

    type 'a fold_f = { f : 'b. 'a -> 'b t -> 'a }
    val fold_left : 'a fold_f -> 'a -> 'b list -> 'a

    type ('a, 's) listmk =
      | [] : ('a, 's) listmk
      | (::) : ('s Select.source -> 'a t) * ('b, 's) listmk -> ('a * 'b, 's) listmk

    val listmk_to_list : 's Select.source -> ('a, 's) listmk -> 'a list

  end = struct
    type 'a t = ..
    type 'a t +=
      | Bool : bool -> bool t
      | Int : int -> int t
      | Float : float -> float t
      | String : string -> string t
      | Blob : bytes -> bytes t
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
      | Is_not_null : 'a t -> bool t
      | Is_null : 'a t -> bool t
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
      | Field : ('t, 'a) Field.t * 's1 Select.source * ('s1, 't1, 't, 's2) Select.steps -> 'a t
      | Foreign : ('t, 't2) Field.foreign_key * 's1 Select.source * ('s1, 't1, 't, 's2) Select.steps -> 'a t
      | Select : 's Select.t -> 'a t

    let bool b = fun _ -> Bool b
    let int i = fun _ -> Int i
    let float x = fun _ -> Float x
    let string s = fun _ -> String s
    let blob b = fun _ -> Blob b
    let field fld steps = fun src -> Field (fld, src, steps)
    let foreign_key fk steps = fun src -> Foreign (fk, src, steps)
    let subquery sel = fun src -> Select sel

    type handover = { handover : 'a. build_step -> 'a t -> build_step }

    let build_param st p =
      { repr = D.placeholder st.pos
      ; params = [p]
      ; pos = st.pos + 1
      }

    let rec build
      : type a. handover:handover -> build_step -> a t -> build_step =
      fun ~handover st -> function
        | Bool b -> build_param st (Param.Bool b)
        | Int i -> build_param st (Param.Int i)
        | Float x -> build_param st (Param.Float x)
        | String s -> build_param st (Param.String s)
        | Blob b -> build_param st (Param.Blob b)
        | Eq (e1, e2) -> build_binop ~handover st "=" e1 e2
        | Neq (e1, e2) -> build_binop ~handover st "<>" e1 e2
        | Gt (e1, e2) -> build_binop ~handover st ">" e1 e2
        | Ge (e1, e2) -> build_binop ~handover st ">=" e1 e2
        | Lt (e1, e2) -> build_binop ~handover st "<" e1 e2
        | Le (e1, e2) -> build_binop ~handover st "<=" e1 e2
        | And (e1, e2) -> build_binop ~handover st "AND" e1 e2
        | Or (e1, e2) -> build_binop ~handover st "OR" e1 e2
        | IAdd (e1, e2) -> build_binop ~handover st "+" e1 e2
        | ISub (e1, e2) -> build_binop ~handover st "-" e1 e2
        | IMul (e1, e2) -> build_binop ~handover st "*" e1 e2
        | IDiv (e1, e2) -> build_binop ~handover st "/" e1 e2
        | FAdd (e1, e2) -> build_binop ~handover st "+" e1 e2
        | FSub (e1, e2) -> build_binop ~handover st "-" e1 e2
        | FMul (e1, e2) -> build_binop ~handover st "*" e1 e2
        | FDiv (e1, e2) -> build_binop ~handover st "/" e1 e2
        | LShift (e, i) ->
            let st = build ~handover st e in
            { repr = sprintf "(%s) << %s" st.repr (D.placeholder st.pos)
            ; params = st.params @ [Param.Int i]
            ; pos = st.pos + 1
            }
        | RShift (e, i) ->
            let st = build ~handover st e in
            { repr = sprintf "(%s) >> %s" st.repr (D.placeholder st.pos)
            ; params = st.params @ [Param.Int i]
            ; pos = st.pos + 1
            }
        | Like (e, pat) ->
            let st = build ~handover st e in
            { repr = sprintf "(%s) LIKE %s" st.repr (D.placeholder st.pos)
            ; params = st.params @ [Param.String pat]
            ; pos = st.pos + 1
            }
        | Not_like (e, pat) ->
            let st = build ~handover st e in
            { repr = sprintf "(%s) NOT LIKE %s" st.repr (D.placeholder st.pos)
            ; params = st.params @ [Param.String pat]
            ; pos = st.pos + 1
            }
        | In (e, l) ->
            let st1 = build ~handover st e in
            let st2 = build_function ~handover st1 (st1.repr ^ " IN(") l ")" in
            { st2 with params = st1.params @ st2.params }
        | Not_in (e, l) ->
            let st1 = build ~handover st e in
            let st2 = build_function ~handover st1 (st1.repr ^ " NOT IN(") l ")" in
            { st2 with params = st1.params @ st2.params }
        | Is_not_null e ->
            build_function ~handover st "" [e] " IS NOT NULL"
        | Is_null e ->
            build_function ~handover st "" [e] " IS NULL"
        | Field (fld, _, _) ->
            { repr = Field.to_string fld
            ; params = []
            ; pos = st.pos
            }
        | Foreign ((fld, _), _, _) ->
            { repr = Field.to_string fld
            ; params = []
            ; pos = st.pos
            }
        | Select s ->
            let repr, params = Select.seal s in
            { repr = "(" ^ repr ^ ")"
            ; params
            ; pos = st.pos
            }
        | e ->
            handover.handover st e

    and build_binop
      : type a b. handover:handover -> build_step -> string -> a t -> b t
               -> build_step =
      fun ~handover st op e1 e2 ->
        let st1 = build ~handover st e1 in
        let st2 = build ~handover st1 e2 in
        { repr = sprintf "(%s) %s (%s)" st1.repr op st2.repr
        ; params = st1.params @ st2.params
        ; pos = st2.pos
        }

    and build_function
      : type a. handover:handover -> build_step
             -> string -> a t list -> string
             -> build_step =
      fun ~handover st left l right ->
        let st = build_arg_list ~handover st l in
        { st with repr = left ^ st.repr ^ right }

    and build_arg_list
      : type a. handover:handover -> build_step -> a t list -> build_step =
      fun ~handover st l ->
        fst @@ List.fold_left
          (fun (acc, i) e ->
            let st = build ~handover st e in
            let repr =
              if i = 0 then begin
                st.repr
              end else
                sprintf "%s, %s" acc.repr st.repr in
            let params = acc.params @ st.params in
            ({ repr; params; pos = st.pos }, i + 1))
          ({ blank_step with pos = st.pos }, 0)
          l

    let (=)         f g = fun src -> Eq (f src, g src)
    let (=%)        f s = fun src -> Like (f src, s)
    let (<>)        f g = fun src -> Neq (f src, g src)
    let (<>%)       f s = fun src -> Not_like (f src, s)
    let (>)         f g = fun src -> Gt (f src, g src)
    let (>=)        f g = fun src -> Ge (f src, g src)
    let (<)         f g = fun src -> Lt (f src, g src)
    let (<=)        f g = fun src -> Le (f src, g src)
    let (&&)        f g = fun src -> And (f src, g src)
    let (||)        f g = fun src -> And (f src, g src)
    let (+)         f g = fun src -> IAdd (f src, g src)
    let (-)         f g = fun src -> ISub (f src, g src)
    let ( * )       f g = fun src -> IMul (f src, g src)
    let (/)         f g = fun src -> IDiv (f src, g src)
    let (+.)        f g = fun src -> FAdd (f src, g src)
    let (-.)        f g = fun src -> FSub (f src, g src)
    let ( *. )      f g = fun src -> FMul (f src, g src)
    let (/.)        f g = fun src -> FDiv (f src, g src)
    let (<<)        f i = fun src -> LShift (f src, i)
    let (>>)        f i = fun src -> RShift (f src, i)
    let (=?)        f l = fun src -> In (f src, List.map (fun f -> f src) l)
    let (<>?)       f l = fun src -> Not_in (f src, List.map (fun f -> f src) l)
    let is_null     f   = fun src -> Is_null (f src)
    let is_not_null f   = fun src -> Is_not_null (f src)

    type _ list =
      | [] : 'a list
      | (::) : 'a t * 'b list -> ('a * 'b) list

    type 'a fold_f = { f : 'b. 'a -> 'b t -> 'a }

    let rec fold_left : type b. 'a fold_f -> 'a -> b list -> 'a = fun f z -> function
      | [] -> z
      | e::es -> fold_left f (f.f z e) es

    type ('a, 's) listmk =
      | [] : ('a, 's) listmk
      | (::) : ('s Select.source -> 'a t) * ('b, 's) listmk -> ('a * 'b, 's) listmk

    type 's map_f = { m : 'a. ('s Select.source -> 'a t) -> 'a t }

    let rec mapmk : type a. 's map_f -> (a, 's) listmk -> a list = fun f -> function
      | [] -> []
      | e::es -> (f.m e)::mapmk f es

    let listmk_to_list src = mapmk { m = fun f -> f src }
  end
end
