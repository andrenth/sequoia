open Printf
open Common

module type S = sig
  type _ t

  val seal : handover:Expr.handover -> 't t -> string * Param.t list

  type ('t, 'a) mk = ('t, 'a) Field.t * ('t Table.t -> ('t, 'a) Expr.expr)

  module OrderBy : sig
    type order

    type ('t, 'a) expr = 'a Expr.t * order

    module Expr : sig
      type ('t, 'a) mk = 't Table.t -> 'a Expr.t * order

      module Vector : Vector.S with type ('t, 'a) elem := ('t, 'a) mk

      val asc : ('t Table.t -> 'a Expr.t) -> 't Table.t -> ('t, 'a) expr
      val desc : ('t Table.t -> 'a Expr.t) -> 't Table.t -> ('t, 'a) expr
    end

    module Vector : Vector.S with type ('t, 'a) elem := ('t, 'a) expr

    val vectormk_to_vector : 't Table.t -> ('t, 'a, 'n) Expr.Vector.t
                          -> ('t, 'a, 'n) Vector.t
  end

  val where : ('t Table.t -> 'a Expr.t) -> 't t -> 't t
  val order_by : ('t, 'a, 'n Nat.s) OrderBy.Expr.Vector.t -> 't t -> 't t
  val limit : ?offset:int -> int -> 't t -> 't t

  module Expr : sig
    include module type of Query_common.UpdateDeleteExpr

    type ('t, 'a) mk = 't Table.t -> ('t, 'a) Expr.expr

    module Vector : Vector.S with type ('t, 'a) elem := ('t, 'a) mk
  end

  module Vector : Vector.S with type ('t, 'a) elem := ('t, 'a) mk

  val update : 't Table.t -> set:('t, 'a, 'n Nat.s) Vector.t -> 't t
end

module Make (D : Driver.S) : S = struct
  open Query_common

  module OrderBy = struct
    type order =
      | Asc
      | Desc

    type ('s, 'a) expr = 'a Expr.t * order

    module Expr = struct
      type ('t, 'a) mk = 't Table.t -> 'a Expr.t * order

      module Vector = Vector.Make(struct
        type ('t, 'a) elem = ('t, 'a) mk
      end)

      let asc f = fun src -> (f src, Asc)
      let desc f = fun src -> (f src, Desc)
    end

    module Vector = Vector.Make(struct
      type ('t, 'a) elem = ('t, 'a) expr
    end)

    let rec vectormk_to_vector
      : type a n. 't Table.t
               -> ('t, a, n) Expr.Vector.t
               -> ('t, a, n) Vector.t =
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

  module E = struct
    include UpdateDeleteExpr

    module Vec = Vector.Make(struct
      type ('t, 'a) elem = ('t, 'a) Expr.expr
    end)

    type ('t, 'a) mk = 't Table.t -> ('t, 'a) Expr.expr

    module Vector = Vector.Make(struct
      type ('t, 'a) elem = ('t, 'a) mk
    end)

    (* let rec vectormk_to_vector
      : type a n. 't Table.t
               -> ('t, a, n) Vector.t
               -> ('t, a, n) Vec.t =
      fun table vec ->
        let open Vector in
        match vec with
        | [] ->
            let open Vec in
            []
        | f::rest ->
            let open Vec in
            (f table) :: vectormk_to_vector table rest *)
  end

  module UpdateVec = Vector.Make(struct
    type ('t, 'a) elem = ('t, 'a) Field.t * ('t, 'a) Expr.expr
  end)

  type ('t, 'a) mk = ('t, 'a) Field.t * ('t Table.t -> ('t, 'a) Expr.expr)

  module Vector = Vector.Make(struct
    type ('t, 'a) elem = ('t, 'a) mk
  end)

  type 't t =
    | U :
        { table    : 't Table.t
        ; updates  : ('t, _, 'n Nat.s) UpdateVec.t
        ; where    : _ Expr.t option
        ; order_by : (_, _, 'm Nat.s) OrderBy.Vector.t option
        ; limit    : (int * int) option
        } -> 't t

  let rec vectormk_to_vector
    : type a n. 't Table.t
             -> ('t, a, n) Vector.t
             -> ('t, a, n) UpdateVec.t =
    fun table vec ->
      let open Vector in
      match vec with
      | [] ->
          let open UpdateVec in
          []
      | (fld, f)::rest ->
          let open UpdateVec in
          (fld, (f table)) :: vectormk_to_vector table rest

  let update table ~set =
    U
     { table
     ; updates = vectormk_to_vector table set
     ; where = None
     ; limit = None
     ; order_by = None
     }

  let where expr (U stmt) =
    U { stmt with where = Some (expr stmt.table) }

  let limit ?(offset = 0) n (U stmt) =
    U { stmt with limit = Some (offset, n) }

  let order_by
    : type a u n. (u, a, n Nat.s) OrderBy.Expr.Vector.t -> u t -> u t =
    fun bl (U stmt) ->
      let vec = OrderBy.vectormk_to_vector stmt.table bl in
      U { stmt with order_by = Some vec }

  let build_updates ~handover st _table updates =
    let fold
      : type t a b. build_step * int
                 -> (t, a) Field.t * (b, a) Expr.expr
                 -> build_step * int =
      fun (st, i) (fld, expr) ->
        let fld = Field.to_string fld in
        let st' = E.build ~placeholder:D.placeholder ~handover st expr in
        if i = 0 then
          (st', i)
        else
          let suf = if i = 1 then "" else "," in
          let st =
            { st' with
              repr = sprintf "%s\n%s = %s%s"  st.repr fld st'.repr suf
            ; params = st.params @ st'.params
            ; pos = st'.pos
            } in
          (st, i - 1) in
    let len = UpdateVec.vector_length updates in
    fst @@ UpdateVec.vector_fold_left
      { UpdateVec.f = fold }
      ({ blank_step with pos = st.pos; aliases = st.aliases }, len)
      updates

  let order_to_string = function
    | OrderBy.Asc -> "ASC"
    | OrderBy.Desc -> "DESC"

  let join_order_by_exprs ~handover st =
    OrderBy.Vector.vector_fold_left
      { OrderBy.Vector.f = fun (st, i) (e, ord) ->
        let st' = E.build ~placeholder:D.placeholder ~handover st e in
        let st' = { st' with repr = st'.repr ^ " " ^ order_to_string ord } in
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
        { st with repr = "ORDER BY " ^ st.repr }
    | None ->
        { blank_step with pos = st.pos; aliases = st.aliases }

  let seal ~handover (U { table; updates; where; limit; order_by }) =
    let st = { blank_step with pos = 1 } in
    let updates_st = build_updates ~handover st table updates in
    let where_st =
      build_where ~placeholder:D.placeholder ~handover updates_st where in
    let order_by_st = build_order_by ~handover where_st order_by in
    let limit_st = build_limit D.placeholder order_by_st limit in
    let s = sprintf "UPDATE %s SET%s" (Table.name table) updates_st.repr in
    let params = updates_st.params
               @ where_st.params
               @ order_by_st.params
               @ limit_st.params in
    let repr =
      join_lines
      [ s
      ; where_st.repr
      ; limit_st.repr
      ; order_by_st.repr
      ] in
    repr, params

  module Expr = E
end
