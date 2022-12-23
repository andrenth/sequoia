open Printf
open Common

module type S = sig
  type _ t

  type ('s1, 't1, 't2, 's2) join_steps =
    | There : (('t2 -> _) as 's1, 't1, 't2, 't1 -> 's1) join_steps
    | Skip : ('s1, 't1, 't2, 's2) join_steps
          -> ('a -> 's1, 't1, 't2, 'a -> 's2) join_steps

  type join

  type 's source =
    | From : 't Table.t -> ('t -> unit) source
    | Join : join
           * ('t1, 't2) Field.foreign_key
           * 's1 source
           * ('s1, 't1, 't2, 's2) join_steps
          -> 's2 source

  type ('s1, 't1, 't2, 's2) steps =
    | There : (('t2 -> _) as 's1, 't1, 't2, 's1) steps
    | Skip : ('s1, 't1, 't2, 's2) steps
          -> ('a -> 's1, 't1, 't2, 'a -> 's2) steps

  val seal : handover:Expr.handover -> 's t -> string * Param.t list

  module Expr : sig
    type 's select = 's t

    type 'a Expr.t +=
      | Field : ('t, 'a) Field.t * 's1 source * ('s1, 't1, 't, 's2) steps
             -> 'a Expr.t
      | Foreign : ('t, 't2) Field.foreign_key
                * 's1 source
                * ('s1, 't1, 't, 's2) steps
               -> 'a Expr.t
      | Select : 's select -> 'a Expr.t

    type 'a t = 'a Expr.t

    val (-->) : ('s source -> 'a t) -> string -> 's source -> 'a t
    val alias : string -> 's source -> 'a t
    val field : ('t, 'a) Field.t -> ('b, 'c, 't, 'd) steps -> 'b source -> 'a t
    val foreign_key : ('t1, 't2) Field.foreign_key -> ('a, 'b, 't1, 'c) steps -> 'a source -> 'd t

    val subquery : 's select -> 't source -> 'c t

    val unwrap : ('t, 'a option) Field.t -> ('b, 'c, 't, 'd) steps -> 'b source -> 'a t

    type ('s, 'a) mk = 's source -> 'a t

    module Vector : Vector.S with type ('s, 'a) elem := ('s, 'a) mk

    val build : handover:Expr.handover -> build_step -> 'a t -> build_step

    val vectormk_to_vector : 's source -> ('s, 'a, 'n) Vector.t
                          -> ('s, 'a, 'n) Expr.Vector.t
  end

  val select : ?distinct:bool
            -> ('s, 'a, 'n Nat.s) Expr.Vector.t
            -> 's source
            -> 's t

  val from : 't Table.t -> ('t -> unit) source

  val left_join  : ('t1, 't2) Field.foreign_key *
                   ('s1, 't1, 't2, 's2) join_steps
                -> 's1 source
                -> 's2 source
  val right_join : ('t1, 't2) Field.foreign_key *
                   ('s1, 't1, 't2, 's2) join_steps
                -> 's1 source
                -> 's2 source
  val inner_join : ('t1, 't2) Field.foreign_key *
                   ('s1, 't1, 't2, 's2) join_steps
                -> 's1 source
                -> 's2 source

  val self : ('t, int) Field.t
          -> ('t, int) Field.t
          -> ('s1, 't1, 't, 's2) join_steps
          -> ('t, 't) Field.foreign_key * ('s1, 't1, 't, 's2) join_steps

  val this : ('t1, 't2) Field.foreign_key
          -> ('s1, 't1, 't2, 's2) join_steps
          -> ('t1, 't2) Field.foreign_key * ('s1, 't1, 't2, 's2) join_steps
  val that : ('t1, 't2) Field.foreign_key
          -> ('s1, 't2, 't1, 's2) join_steps
          -> ('t2, 't1) Field.foreign_key * ('s1, 't2, 't1, 's2) join_steps

  val where : ('a source -> bool Expr.t) -> 'a t -> 'a t
  val group_by : ?having:('s source -> bool Expr.t)
              -> ('s, 'a, 'n Nat.s) Expr.Vector.t
              -> 's t
              -> 's t

  module OrderBy : sig
    type order

    type ('s, 'a) expr = 'a Expr.t * order

    module Expr : sig
      type ('s, 'a) mk = 's source -> 'a Expr.t * order

      module Vector : Vector.S with type ('s, 'a) elem := ('s, 'a) mk

      val asc : ('s source -> 'a Expr.t) -> 's source -> ('s, 'a) expr
      val desc : ('s source -> 'a Expr.t) -> 's source -> ('s, 'a) expr
    end

    module Vector : Vector.S with type ('s, 'a) elem := ('s, 'a) expr

    val vectormk_to_vector : 's source -> ('s, 'a, 'n) Expr.Vector.t
                          -> ('s, 'a, 'n) Vector.t
  end

  val order_by : ('s, 'a, 'n Nat.s) OrderBy.Expr.Vector.t -> 's t -> 's t
  val limit : ?offset:int -> int -> 'a t -> 'a t
end

module Make (D : Driver.S) : S = struct
  type ('s1, 't1, 't2, 's2) join_steps =
    | There : (('t2 -> _) as 's1, 't1, 't2, 't1 -> 's1) join_steps
    | Skip : ('s1, 't1, 't2, 's2) join_steps
          -> ('a -> 's1, 't1, 't2, 'a -> 's2) join_steps

  type join
    = Left
    | Right
    | Inner

  type 's source =
    | From : 't Table.t -> ('t -> unit) source
    | Join : join
           * ('t1, 't2) Field.foreign_key
           * 's1 source
           * ('s1, 't1, 't2, 's2) join_steps
          -> 's2 source

  type ('s1, 't1, 't2, 's2) steps =
    | There : (('t2 -> _) as 's1, 't1, 't2, 's1) steps
    | Skip : ('s1, 't1, 't2, 's2) steps -> ('a -> 's1, 't1, 't2, 'a -> 's2) steps

  module OrderBy = struct
    type order =
      | Asc
      | Desc

    type ('s, 'a) expr = 'a Expr.t * order

    module Expr = struct
      type ('s, 'a) mk = 's source -> 'a Expr.t * order

      module Vector = Vector.Make(struct
        type ('s, 'a) elem = ('s, 'a) mk
      end)

      let asc f = fun src -> (f src, Asc)
      let desc f = fun src -> (f src, Desc)
    end

    module Vector = Vector.Make(struct
      type ('s, 'a) elem = ('s, 'a) expr
    end)

    let rec vectormk_to_vector
      : type a s n. s source
                 -> (s, a, n) Expr.Vector.t
                 -> (s, a, n) Vector.t =
      fun src vec ->
        let open Expr.Vector in
        match vec with
        | [] ->
            let open Vector in
            []
        | f::fs ->
            let open Vector in
            (f src) :: vectormk_to_vector src fs
  end

  module rec SelectExpr : sig
    type 's select = 's T.t

    type 'a Expr.t +=
      | As : 'a Expr.t * string -> 'a Expr.t
      | Alias : string -> 'a Expr.t
      | Field : ('t, 'a) Field.t * 's1 source * ('s1, 't1, 't, 's2) steps -> 'a Expr.t
      | Foreign : ('t, 't2) Field.foreign_key * 's1 source * ('s1, 't1, 't, 's2) steps -> 'a Expr.t
      | Select : 's T.t -> 'a Expr.t

    type 'a t = 'a Expr.t

    val build : handover:Expr.handover -> build_step -> 'a t -> build_step

    val (-->) : ('s source -> 'a Expr.t) -> string -> 's source -> 'a Expr.t
    val alias : string -> 's source -> 'a Expr.t
    val field : ('t, 'a) Field.t -> ('b, 'c, 't, 'd) steps -> 'b source -> 'a t
    val foreign_key : ('t1, 't2) Field.foreign_key -> ('a, 'b, 't1, 'c) steps -> 'a source -> 'd t

    val subquery : 's T.t -> 't source -> 'c t

    val unwrap : ('t, 'a option) Field.t -> ('b, 'c, 't, 'd) steps -> 'b source -> 'a t

    type ('s, 'a) mk = 's source -> 'a t

    module Vector : Vector.S with type ('s, 'a) elem := ('s, 'a) mk

    val vectormk_to_vector : 's source -> ('s, 'a, 'n) Vector.t
                          -> ('s, 'a, 'n) Expr.Vector.t

  end = struct
    type 's select = 's T.t

    type 'a Expr.t +=
      | As : 'a Expr.t * string -> 'a Expr.t
      | Alias : string -> 'a Expr.t
      | Field : ('t, 'a) Field.t * 's1 source * ('s1, 't1, 't, 's2) steps -> 'a Expr.t
      | Foreign : ('t, 't2) Field.foreign_key * 's1 source * ('s1, 't1, 't, 's2) steps -> 'a Expr.t
      | Select : 's T.t -> 'a Expr.t

    type 'a t = 'a Expr.t

    let (-->) expr alias = fun src -> As (expr src, alias)
    let alias name = fun _src -> Alias name
    let field fld steps = fun src -> Field (fld, src, steps)
    let foreign_key fk steps = fun src -> Foreign (fk, src, steps)
    let subquery sel = fun _src -> Select sel

    (* XXX is there a better name for this? *)
    let unwrap
      : type a. ('t, a option) Field.t -> ('b, 'c, 't, 'd) steps
             -> 'b source -> a Expr.t =
      fun fld steps src ->
        match fld with
        | Field.Null.Bool (n, t) -> Field (Field.Bool (n, t), src, steps)
        | Field.Null.Int (n, t) -> Field (Field.Int (n, t), src, steps)
        | Field.Null.Float (n, t) -> Field (Field.Float (n, t), src, steps)
        | Field.Null.String (n, t) -> Field (Field.String (n, t), src, steps)
        | Field.Null.Blob (n, t) -> Field (Field.Blob (n, t), src, steps)
        | _ -> assert false

    let rec build
      : type a. handover:Expr.handover
             -> build_step
             -> a Expr.t
             -> build_step =
      fun ~handover st e ->
        match e with
        | As (expr, alias) ->
            let st = build ~handover st expr in
            { st with
              repr = sprintf "%s AS %s" st.repr alias
            ; aliases = AliasSet.add alias st.aliases
            }
        | Alias name ->
            if AliasSet.mem name st.aliases then
              { st with repr = name; params = [] }
            else
              failwith (sprintf "unknown alias '%s'" name)
        | Field (Field.Bool _ as fld, _, _) ->
            { st with repr = Field.to_string fld; params = [] }
        | Field (Field.Int _ as fld, _, _) ->
            { st with repr = Field.to_string fld; params = [] }
        | Field (Field.Float _ as fld, _, _) ->
            { st with repr = Field.to_string fld; params = [] }
        | Field (Field.String _ as fld, _, _) ->
            { st with repr = Field.to_string fld; params = [] }
        | Field (Field.Blob _ as fld, _, _) ->
            { st with repr = Field.to_string fld; params = [] }
        | Field (Field.Null.Bool _ as fld, _, _) ->
            { st with repr = Field.to_string fld; params = [] }
        | Field (Field.Null.Int _ as fld, _, _) ->
            { st with repr = Field.to_string fld; params = [] }
        | Field (Field.Null.Float _ as fld, _, _) ->
            { st with repr = Field.to_string fld; params = [] }
        | Field (Field.Null.String _ as fld, _, _) ->
            { st with repr = Field.to_string fld; params = [] }
        | Field (Field.Null.Blob _ as fld, _, _) ->
            { st with repr = Field.to_string fld; params = [] }
        | Foreign ((fld, _), _, _) ->
            { st with repr = Field.to_string fld; params = [] }
        | Select s ->
            let repr, params = T.seal ~handover s in
            { st with repr = "(" ^ repr ^ ")"; params }
        | e ->
            Expr.build ~placeholder:D.placeholder ~handover st e

    type ('s, 'a) mk = 's source -> 'a Expr.t

    module Vector = Vector.Make(struct
      type ('s, 'a) elem = ('s, 'a) mk
    end)

    let rec vectormk_to_vector
      : type a s n. s source
                 -> (s, a, n) Vector.t
                 -> (s, a, n) Expr.Vector.t =
      fun src vec ->
        let open Vector in
        match vec with
        | [] ->
            let open Expr.Vector in
            []
        | f::fs ->
            let open Expr.Vector in
            (f src) :: vectormk_to_vector src fs
  end

  and T : sig
    type _ t =
      | S :
          { source   : 's source
          ; distinct : bool
          ; select   : (_, _, 'n Nat.s) Expr.Vector.t
          ; where    : _ Expr.t option
          ; group_by : ((_, _, 'm Nat.s) Expr.Vector.t * bool Expr.t option) option
          ; order_by : (_, _, 'k Nat.s) OrderBy.Vector.t option
          ; limit    : (int * int) option
          } -> 's t

    val seal : handover:Expr.handover -> 's t -> string * Param.t list
  end = struct
    module E = Expr
    module Expr = SelectExpr

    type _ t =
      | S :
          { source   : 's source
          ; distinct : bool
          ; select   : (_, _, 'n Nat.s) E.Vector.t
          ; where    : _ Expr.t option
          ; group_by : ((_, _, 'm Nat.s) E.Vector.t * bool Expr.t option) option
          ; order_by : (_, _, 'k Nat.s) OrderBy.Vector.t option
          ; limit    : (int * int) option
          } -> 's t

    let join_to_string = function
      | Left -> "LEFT"
      | Right -> "RIGHT"
      | Inner -> "INNER"

    let join_exprs ~handover st =
      E.Vector.vector_fold_left
        { E.Vector.f = fun (st, i) e ->
          let st' = Expr.build ~handover st e in
          if i = 0 then
            (st', 1)
          else
            let st =
              { st' with
                repr = st.repr ^ ", " ^ st'.repr
              ; params = st.params @ st'.params
              ; pos = st'.pos
              } in
            (st, i + 1) }
        ({ blank_step with pos = st.pos; aliases = st.aliases }, 0)

    let build_select ~handover st exprs =
      fst (join_exprs ~handover st exprs)

    let rec build_source
      : type s. ?distinct:bool
             -> handover:E.handover
             -> build_step
             -> ('a, _, 'n) E.Vector.t -> s source
             -> build_step =
      fun ?(distinct = false) ~handover st exprs -> function
        | From t ->
            let st = build_select ~handover st exprs in
            let repr =
              sprintf "SELECT%s%s\nFROM %s"
                (if distinct then " DISTINCT " else " ")
                st.repr
                (Table.name t) in
            { st with
              repr
            ; params = st.params
            ; pos = st.pos
            }
        | Join (join, (a, b), src, _) ->
            let st = build_source ~distinct ~handover st exprs src in
            let repr =
              st.repr
                ^ "\n"
                ^ sprintf "%s JOIN %s ON %s = %s"
                    (join_to_string join)
                    (Table.name (Field.table a))
                    (Field.to_string a)
                    (Field.to_string b) in
            { st with
              repr
            ; params = st.params
            ; pos = st.pos
            }

    let build_group_by
      : type t a n. handover:E.handover
             -> build_step
             -> ((t, a , n Nat.s) E.Vector.t * bool Expr.t option) option
             -> build_step =
      fun ~handover st -> function
        | Some (flds, having) ->
            let st = fst (join_exprs ~handover st flds) in
            begin match having with
            | Some expr ->
                let st' = Expr.build ~handover st expr in
                let repr =
                  sprintf "GROUP BY (%s) HAVING (%s)" st.repr st'.repr in
                { st' with repr }
            | None ->
              { st with repr = sprintf "GROUP BY (%s)" st.repr }
            end
        | None ->
            { blank_step with pos = st.pos; aliases = st.aliases }

    let order_to_string = function
      | OrderBy.Asc -> "ASC"
      | OrderBy.Desc -> "DESC"

    let join_order_by_exprs ~handover st =
      OrderBy.Vector.vector_fold_left
        { OrderBy.Vector.f = fun (st, i) (e, ord) ->
          let st' = Expr.build ~handover st e in
          let st' =
            { st' with repr = st'.repr ^ " " ^ order_to_string ord } in
          if i = 0 then
            (st', 1)
          else
            let st =
              { st' with
                repr = st.repr ^ ", " ^ st'.repr
              ; params = st.params @ st'.params
              ; pos = st'.pos
              } in
            (st, i + 1) }
        ({ blank_step with pos = st.pos; aliases = st.aliases }, 0)

    let build_order_by ~handover st = function
      | Some exprs ->
          let st = fst (join_order_by_exprs ~handover st exprs) in
          { st with repr = sprintf "ORDER BY %s" st.repr }
      | None ->
          { blank_step with pos = st.pos; aliases = st.aliases }

    open Query_common

    let seal : type s. handover:E.handover -> s t -> string * Param.t list =
      fun ~handover (S stmt) ->
        let st = { blank_step with pos = 1 } in
        let src_st =
          build_source
            ~distinct:stmt.distinct ~handover st stmt.select stmt.source in
        let where_st =
          build_where ~placeholder:D.placeholder ~handover src_st stmt.where in
        let group_by_st = build_group_by ~handover where_st stmt.group_by in
        let order_by_st = build_order_by ~handover group_by_st stmt.order_by in
        let limit_st = build_limit D.placeholder order_by_st stmt.limit in
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
  end

  include T
  module Expr = SelectExpr

  let select
    : type s a. ?distinct:bool
   -> (s, a, 'n Nat.s) Expr.Vector.t
   -> s source
   -> s t =
    fun ?(distinct = false) bl src ->
      S
        { source = src
        ; distinct
        ; select = Expr.vectormk_to_vector src bl
        ; where = None
        ; group_by = None
        ; order_by = None
        ; limit = None
        }

  let from t =
    From t

  let join kind rel steps src =
    Join (kind, rel, src, steps)

  let left_join  (rel, steps) src = join Left  rel steps src
  let right_join (rel, steps) src = join Right rel steps src
  let inner_join (rel, steps) src = join Inner rel steps src

  let self fld1 fld2 steps = ((fld1, fld2), steps)
  let this rel steps = (rel, steps)
  let that (fk, pk) steps = ((pk, fk), steps)

  let where expr (S stmt) =
    S { stmt with where = Some (expr stmt.source) }

  let group_by
    : type a s n. ?having:(s source -> bool Expr.t)
               -> (s, a, n Nat.s) Expr.Vector.t
               -> s t
               -> s t =
    fun ?having flds (S stmt) ->
      let flds = Expr.vectormk_to_vector stmt.source flds in
      let having =
        match having with
        | Some f -> Some (f stmt.source)
        | None -> None in
      S { stmt with group_by = Some (flds, having) }

  let order_by
    : type a s n. (s, a, n Nat.s) OrderBy.Expr.Vector.t -> s t -> s t =
    fun bl (S stmt) ->
      let vec = OrderBy.vectormk_to_vector stmt.source bl in
      S { stmt with order_by = Some vec }

  let limit ?(offset = 0) n (S stmt) =
    S { stmt with limit = Some (offset, n) }
end
