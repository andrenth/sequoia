open Printf

module Nat = struct
  type z
  type 'n s = S
end

module type Elem = sig
  type ('a, 'b) elem
end

module type VECTOR = sig
  type ('a, 'b) elem

  type (_, _, _) t =
    | [] : ('a, 'b, Nat.z) t
    | (::) : ('a, 'b) elem * ('a, 'c, 'n) t -> ('a, 'b * 'c, 'n Nat.s) t

  type 'z folder = { f : 'a 'b. 'z -> ('a, 'b) elem -> 'z }

  val vector_fold_left : 'z folder -> 'z -> ('a, 'b, 'n) t -> 'z
  val vector_length : ('a, 'b, 'n) t -> int

  type ('a, 'b, 'm, 'n) matrix =
    | [] : ('a, 'b, Nat.z, 'n) matrix
    | (::) : ('a, 'b, 'n) t * ('a, 'b, 'm, 'n) matrix
          -> ('a, 'b, 'm Nat.s, 'n) matrix

  val matrix_fold_left : 'z folder -> 'z -> ('a, 'b, 'm, 'n) matrix -> 'z
end

module Vector = struct
  module Make (E : Elem) : VECTOR
    with type ('a, 'b) elem := ('a, 'b) E.elem =
  struct
    type (_, _, _) t =
      | [] : ('a, 'b, Nat.z) t
      | (::) : ('a, 'b) E.elem * ('a, 'c, 'n) t -> ('a, 'b * 'c, 'n Nat.s) t

    type 'z folder = { f : 'a 'b. 'z -> ('a, 'b) E.elem -> 'z }

    let rec vector_fold_left
      : type a b n. 'z folder -> 'z -> (a, b, n) t -> 'z =
      fun f z -> function
        | [] -> z
        | e::es -> vector_fold_left f (f.f z e) es

    let rec vector_length : type a b n. (a, b, n) t -> int = function
      | [] -> 0
      | _::xs -> 1 + vector_length xs

    type ('a, 'b, 'm, 'n) matrix =
      | [] : ('a, 'b, Nat.z, 'n) matrix
      | (::) : ('a, 'b, 'n) t * ('a, 'b, 'm, 'n) matrix
            -> ('a, 'b, 'm Nat.s, 'n) matrix

    type 'z matrix_folder = { mf : 'a 'b. 'z -> ('a, 'b) E.elem -> 'z }

    let rec matrix_fold_left
      : type a b m n. 'z folder -> 'z -> (a, b, m, n) matrix -> 'z =
      fun f z -> function
        | [] -> z
        | v::vs ->
            let z = vector_fold_left f z v in
            matrix_fold_left f z vs
  end
end

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

  module Lit = struct
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

    let build_param st p =
      { repr = D.placeholder st.pos
      ; params = [p]
      ; pos = st.pos + 1
      }

    let build : type a. build_step -> a t -> build_step =
      fun st -> function
        | Bool b -> build_param st (Param.Bool b)
        | Int i -> build_param st (Param.Int i)
        | Float x -> build_param st (Param.Float x)
        | String s -> build_param st (Param.String s)
        | Blob b -> build_param st (Param.Blob b)
        | Null.Bool b -> build_param st (Param.Bool b)
        | Null.Int i -> build_param st (Param.Int i)
        | Null.Float x -> build_param st (Param.Float x)
        | Null.String s -> build_param st (Param.String s)
        | Null.Blob b -> build_param st (Param.Blob b)
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

    module Vector = Vector.Make(struct
      type ('a, 'b) elem = 'b t
    end)
  end

  module Expr = struct
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

    type handover = { handover : 'a. build_step -> 'a t -> build_step }

    let rec build
      : type a. handover:handover -> build_step -> a t -> build_step =
      fun ~handover st e ->
        match e with
        | Base.Lit ((Lit.Bool _) as lit) -> Lit.build st lit
        | Base.Lit ((Lit.Int _) as lit) -> Lit.build st lit
        | Base.Lit ((Lit.Float _) as lit) -> Lit.build st lit
        | Base.Lit ((Lit.String _) as lit) -> Lit.build st lit
        | Base.Lit ((Lit.Blob _) as lit) -> Lit.build st lit
        | Base.Lit ((Lit.Null.Bool _) as lit) -> Lit.build st lit
        | Base.Lit ((Lit.Null.Int _) as lit) -> Lit.build st lit
        | Base.Lit ((Lit.Null.Float _) as lit) -> Lit.build st lit
        | Base.Lit ((Lit.Null.String _) as lit) -> Lit.build st lit
        | Base.Lit ((Lit.Null.Blob _) as lit) -> Lit.build st lit
        | Base.Eq (e1, e2) -> build_binop ~handover st "=" e1 e2
        | Base.Neq (e1, e2) -> build_binop ~handover st "<>" e1 e2
        | Base.Gt (e1, e2) -> build_binop ~handover st ">" e1 e2
        | Base.Ge (e1, e2) -> build_binop ~handover st ">=" e1 e2
        | Base.Lt (e1, e2) -> build_binop ~handover st "<" e1 e2
        | Base.Le (e1, e2) -> build_binop ~handover st "<=" e1 e2
        | Base.And (e1, e2) -> build_binop ~handover st "AND" e1 e2
        | Base.Or (e1, e2) -> build_binop ~handover st "OR" e1 e2
        | Base.IAdd (e1, e2) -> build_binop ~handover st "+" e1 e2
        | Base.ISub (e1, e2) -> build_binop ~handover st "-" e1 e2
        | Base.IMul (e1, e2) -> build_binop ~handover st "*" e1 e2
        | Base.IDiv (e1, e2) -> build_binop ~handover st "/" e1 e2
        | Base.FAdd (e1, e2) -> build_binop ~handover st "+" e1 e2
        | Base.FSub (e1, e2) -> build_binop ~handover st "-" e1 e2
        | Base.FMul (e1, e2) -> build_binop ~handover st "*" e1 e2
        | Base.FDiv (e1, e2) -> build_binop ~handover st "/" e1 e2
        | Base.LShift (e, i) ->
            let st = build ~handover st e in
            { repr = sprintf "(%s) << %s" st.repr (D.placeholder st.pos)
            ; params = st.params @ [Param.Int i]
            ; pos = st.pos + 1
            }
        | Base.RShift (e, i) ->
            let st = build ~handover st e in
            { repr = sprintf "(%s) >> %s" st.repr (D.placeholder st.pos)
            ; params = st.params @ [Param.Int i]
            ; pos = st.pos + 1
            }
        | Base.Like (e, pat) ->
            let st = build ~handover st e in
            { repr = sprintf "(%s) LIKE %s" st.repr (D.placeholder st.pos)
            ; params = st.params @ [Param.String pat]
            ; pos = st.pos + 1
            }
        | Base.Not_like (e, pat) ->
            let st = build ~handover st e in
            { repr = sprintf "(%s) NOT LIKE %s" st.repr (D.placeholder st.pos)
            ; params = st.params @ [Param.String pat]
            ; pos = st.pos + 1
            }
        | Base.In (e, l) ->
            let st1 = build ~handover st e in
            let st2 = build_function ~handover st1 (st1.repr ^ " IN(") l ")" in
            { st2 with params = st1.params @ st2.params }
        | Base.Not_in (e, l) ->
            let st1 = build ~handover st e in
            let st2 = build_function ~handover st1 (st1.repr ^ " NOT IN(") l ")" in
            { st2 with params = st1.params @ st2.params }
        | Base.Is_not_null e ->
            build_function ~handover st "" [e] " IS NOT NULL"
        | Base.Is_null e ->
            build_function ~handover st "" [e] " IS NULL"
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
      : type a. handover:handover -> build_step -> string -> a t list -> string
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

    type ('a, 'b) expr = 'b t

    module Vector = Vector.Make(struct
      type ('a, 'b) elem = ('a, 'b) expr
    end)
  end

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

    module Null = struct
      type ('t, 'a) t +=
        | Bool : string * 't Table.t -> ('t, bool option) t
        | Int : string * 't Table.t -> ('t, int option) t
        | Float : string * 't Table.t -> ('t, float option) t
        | String : string * 't Table.t -> ('t, string option) t
        | Blob : string * 't Table.t -> ('t, bytes option) t

      let bool table name = Bool (name, table)
      let int table name = Int (name, table)
      let float table name = Float (name, table)
      let string table name = String (name, table)
      let blob table name = Blob (name, table)
    end

    type ('t1, 't2) foreign_key = ('t1, int) t * ('t2, int) t

    let name : type a. ('t, a) t -> string = function
      | Bool (n, _) -> n
      | Int (n, _) -> n
      | Float (n, _) -> n
      | String (n, _) -> n
      | Blob (n, _) -> n
      | Null.Bool (n, _) -> n
      | Null.Int (n, _) -> n
      | Null.Float (n, _) -> n
      | Null.String (n, _) -> n
      | Null.Blob (n, _) -> n
      | _ -> assert false

    let table : type u a. (u, a) t -> u Table.t = function
      | Bool (_, t) -> t
      | Int (_, t) -> t
      | Float (_, t) -> t
      | String (_, t) -> t
      | Blob (_, t) -> t
      | Null.Bool (_, t) -> t
      | Null.Int (_, t) -> t
      | Null.Float (_, t) -> t
      | Null.String (_, t) -> t
      | Null.Blob (_, t) -> t
      | _ -> assert false

    let to_string fld =
      let t = table fld in
      sprintf "%s.%s" (t.Table.name) (name fld)

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

      val bool : string -> bool t
      val int : string -> int t
      val float : string -> float t
      val string : string -> string t
      val blob : string -> bytes t

      val foreign_key : string -> references:('t, int) Field.t -> 't foreign_key

      module Null : sig
        val bool : string -> bool option t
        val int : string -> int option t
        val float : string -> float option t
        val string : string -> string option t
        val blob : string -> bytes option t
      end
    end
  end

  module MakeTable (T: NAMED) : TABLE = struct
    type t = T.t

    let table = Table.called T.name

    module Field = struct
      type table = t
      type 'a t = (table, 'a) Field.t
      type 't foreign_key = (table, 't) Field.foreign_key

      let bool = Field.bool table
      let int = Field.int table
      let float = Field.float table
      let string = Field.string table
      let blob = Field.blob table

      let foreign_key name ~references =
        Field.foreign_key table name ~references

      module Null = struct
        let bool = Field.Null.bool table
        let int = Field.Null.int table
        let float = Field.Null.float table
        let string = Field.Null.string table
        let blob = Field.Null.blob table
      end
    end
  end

  let table name =
    (module struct
      include MakeTable (struct type t let name = name end)
    end : TABLE)

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

  let build_where
    : type a. handover:Expr.handover
           -> build_step
           -> a Expr.t option
           -> build_step =
    fun ~handover st -> function
      | Some expr ->
          let st = Expr.build ~handover st expr in
          { st with repr = sprintf "WHERE (%s)" st.repr }
      | None ->
          { blank_step with pos = st.pos }

  let build_limit st = function
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



  module Select : sig
    type _ t

    type 's source

    type ('s1, 't1, 't2, 's2) join_steps =
      | There : (('t2 -> _) as 's1, 't1, 't2, 't1 -> 's1) join_steps
      | Skip : ('s1, 't1, 't2, 's2) join_steps -> ('a -> 's1, 't1, 't2, 'a -> 's2) join_steps

    type ('s1, 't1, 't2, 's2) steps =
      | There : (('t2 -> _) as 's1, 't1, 't2, 's1) steps
      | Skip : ('s1, 't1, 't2, 's2) steps -> ('a -> 's1, 't1, 't2, 'a -> 's2) steps

    val seal : handover:Expr.handover -> 's t -> string * Param.t list

    module Expr : sig
      type 's select = 's t

      type 'a Expr.t +=
        | Field : ('t, 'a) Field.t * 's1 source * ('s1, 't1, 't, 's2) steps -> 'a Expr.t
        | Foreign : ('t, 't2) Field.foreign_key * 's1 source * ('s1, 't1, 't, 's2) steps -> 'a Expr.t
        | Select : 's select -> 'a Expr.t

      type 'a t = 'a Expr.t

      val build : handover:Expr.handover -> build_step -> 'a t -> build_step

      val field : ('t, 'a) Field.t -> ('b, 'c, 't, 'd) steps -> 'b source -> 'a t
      val foreign_key : ('t1, 't2) Field.foreign_key -> ('a, 'b, 't1, 'c) steps -> 'a source -> 'd t

      val subquery : 's select -> 't source -> 'c t

      val unwrap : ('t, 'a option) Field.t -> ('b, 'c, 't, 'd) steps -> 'b source -> 'a t

      type ('s, 'a) mk = 's source -> 'a t

      module Vector :
        VECTOR with type ('s, 'a) elem := ('s, 'a) mk

      val vectormk_to_vector : 's source -> ('s, 'a, 'n) Vector.t
                            -> ('s, 'a, 'n) Expr.Vector.t
    end

    val select : ('s, 'a, 'n Nat.s) Expr.Vector.t -> 's source -> 's t

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

    val having_one : ('t1, 't2) Field.foreign_key
                  -> ('s1, 't1, 't2, 's2) join_steps
                  -> ('t1, 't2) Field.foreign_key *
                     ('s1, 't1, 't2, 's2) join_steps
    val belonging_to : ('t1, 't2) Field.foreign_key
                    -> ('s1, 't2, 't1, 's2) join_steps
                    -> ('t2, 't1) Field.foreign_key *
                       ('s1, 't2, 't1, 's2) join_steps

    val where : ('a source -> 'b Expr.t) -> 'a t -> 'a t
    val group_by : ('s source -> 'a Expr.t) -> 's t -> 's t
    val order_by : ('s, 'a, 'n Nat.s) Expr.Vector.t -> 's t -> 's t
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

    module rec SelectExpr : sig
      type 's select = 's T.t

      type 'a Expr.t +=
        | Field : ('t, 'a) Field.t * 's1 source * ('s1, 't1, 't, 's2) steps -> 'a Expr.t
        | Foreign : ('t, 't2) Field.foreign_key * 's1 source * ('s1, 't1, 't, 's2) steps -> 'a Expr.t
        | Select : 's T.t -> 'a Expr.t

      type 'a t = 'a Expr.t

      val build : handover:Expr.handover -> build_step -> 'a t -> build_step

      val field : ('t, 'a) Field.t -> ('b, 'c, 't, 'd) steps -> 'b source -> 'a t
      val foreign_key : ('t1, 't2) Field.foreign_key -> ('a, 'b, 't1, 'c) steps -> 'a source -> 'd t

      val subquery : 's T.t -> 't source -> 'c t

      val unwrap : ('t, 'a option) Field.t -> ('b, 'c, 't, 'd) steps -> 'b source -> 'a t

      type ('s, 'a) mk = 's source -> 'a t

      module Vector :
        VECTOR with type ('s, 'a) elem := ('s, 'a) mk

      val vectormk_to_vector : 's source -> ('s, 'a, 'n) Vector.t
                            -> ('s, 'a, 'n) Expr.Vector.t

    end = struct
      type 's select = 's T.t

      type 'a Expr.t +=
        | Field : ('t, 'a) Field.t * 's1 source * ('s1, 't1, 't, 's2) steps -> 'a Expr.t
        | Foreign : ('t, 't2) Field.foreign_key * 's1 source * ('s1, 't1, 't, 's2) steps -> 'a Expr.t
        | Select : 's T.t -> 'a Expr.t

      type 'a t = 'a Expr.t

      let field fld steps = fun src -> Field (fld, src, steps)
      let foreign_key fk steps = fun src -> Foreign (fk, src, steps)
      let subquery sel = fun src -> Select sel

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
              let repr, params = T.seal ~handover s in
              { repr = "(" ^ repr ^ ")"
              ; params
              ; pos = st.pos
              }
          | e ->
              Expr.build ~handover st e

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
            ; select   : ('a, _, 'n Nat.s) Expr.Vector.t
            ; where    : 'b Expr.t option
            ; group_by : 'c Expr.t option
            ; order_by : ('d, _, 'm Nat.s) Expr.Vector.t option
            ; limit    : (int * int) option
            } -> 's t

      val seal : handover:Expr.handover -> 's t -> string * Param.t list
    end = struct
      module E = Expr
      module Expr = SelectExpr

      type _ t =
        | S :
            { source   : 's source
            ; select   : ('a, _, 'n Nat.s) E.Vector.t
            ; where    : 'b Expr.t option
            ; group_by : 'c Expr.t option
            ; order_by : ('d, _, 'm Nat.s) E.Vector.t option
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
                { repr = st.repr ^ ", " ^ st'.repr
                ; params = st.params @ st'.params
                ; pos = st'.pos
                } in
              (st, i + 1) }
          ({ blank_step with pos = st.pos }, 0)

      let build_select ~handover st exprs =
        fst (join_exprs ~handover st exprs)

      let rec build_source
        : type s. handover:E.handover
               -> build_step
               -> ('a, _, 'n) E.Vector.t -> s source
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

      let build_group_by
        : type a. handover:E.handover
               -> build_step
               -> a Expr.t option
               -> build_step =
        fun ~handover st -> function
          | Some expr ->
              let st = Expr.build ~handover st expr in
              { st with repr = sprintf "GROUP BY (%s)" st.repr }
          | None ->
              { blank_step with pos = st.pos }

      let build_order_by ~handover st = function
        | Some exprs ->
            let st = fst (join_exprs ~handover st exprs) in
            { st with repr = "ORDER BY " ^ st.repr }
        | None ->
            st

      let seal : type s. handover:E.handover -> s t -> string * Param.t list =
        fun ~handover (S stmt) ->
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
    end

    include T
    module Expr = SelectExpr

    let select : type s a. (s, a, 'n Nat.s) Expr.Vector.t -> s source -> s t = fun bl src ->
      S
        { source = src
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
    let having_one rel steps = (rel, steps)
    let belonging_to (fk, pk) steps = ((pk, fk), steps)

    let where expr (S stmt) =
      S { stmt with where = Some (expr stmt.source) }

    let group_by expr (S stmt) =
      S { stmt with group_by = Some (expr stmt.source) }

    let order_by
      : type a s n. (s, a, n Nat.s) Expr.Vector.t -> s t -> s t =
      fun bl (S stmt) ->
        let vec = Expr.vectormk_to_vector stmt.source bl in
        S { stmt with order_by = Some vec }

    let limit ?(offset = 0) n (S stmt) =
      S { stmt with limit = Some (offset, n) }
  end

  module Insert = struct
    module Vector = Vector.Make(struct
      type ('t, 'a) elem = ('t, 'a) Field.t
    end)

    type _ t =
      | I :
          { table  : 't Table.t
          ; fields : ('t, 'a, 'n Nat.s) Vector.t
          ; values : ('u, 'a, 'm Nat.s, 'n Nat.s) Lit.Vector.matrix
          } -> 'i t

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

    let build (I { table; fields; values }) =
      let s =
        sprintf "INSERT INTO %s (%s) VALUES\n%s"
          (Table.to_string table)
          (join_fields fields)
          (placeholders values) in
      s, List.rev @@ params_of_values values
  end

  module Update = struct
    module E = struct
      type _ Expr.t +=
        | Field : ('t, 'a) Field.t * 't Table.t -> 'a Expr.t
        | Foreign : ('t, 'u) Field.foreign_key * 't Table.t -> 'a Expr.t

      let field fld = fun table -> Field (fld, table)
      let foreign_key fk = fun table -> Foreign (fk, table)

      let rec build
        : type a. handover:Expr.handover
               -> build_step
               -> a Expr.t
               -> build_step =
        fun ~handover st e ->
          match e with
          | Field (fld, _) ->
              { repr = Field.to_string fld
              ; params = []
              ; pos = st.pos
              }
          | Foreign ((fld, _), _) ->
              { repr = Field.to_string fld
              ; params = []
              ; pos = st.pos
              }
          | e ->
              Expr.build ~handover st e

      module Vec = Vector.Make(struct
        type ('t, 'a) elem = ('t, 'a) Expr.expr
      end)

      module Vector = Vector.Make(struct
        type ('t, 'a) elem = ('t Table.t -> ('t, 'a) Expr.expr)
      end)

      let rec vectormk_to_vector
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
              (f table) :: vectormk_to_vector table rest
    end

    module UpdateVec = Vector.Make(struct
      type ('t, 'a) elem = ('t, 'a) Field.t * ('t, 'a) Expr.expr
    end)

    module Vector = Vector.Make(struct
      type ('t, 'a) elem = ('t, 'a) Field.t * ('t Table.t -> ('t, 'a) Expr.expr)
    end)

    type 't t =
      | U :
          { table    : 't Table.t
          ; updates  : ('t, 'a, 'n Nat.s) UpdateVec.t
          ; where    : 'b Expr.t option
          ; order_by : ('c, _, 'm Nat.s) E.Vec.t option
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
      : type a u n. (u, a, n Nat.s) E.Vector.t -> u t -> u t =
      fun bl (U stmt) ->
        let vec = E.vectormk_to_vector stmt.table bl in
        U { stmt with order_by = Some vec }

    let build_updates ~handover st table updates =
      let fold
        : type t a b. build_step * int
                   -> (t, a) Field.t * (b, a) Expr.expr
                   -> build_step * int =
        fun (st, i) (fld, expr) ->
          let fld = Field.to_string fld in
          let st' = E.build ~handover st expr in
          if i = 0 then
            (st', i)
          else
            let suf = if i = 1 then "" else "," in
            let st =
              { repr = sprintf "%s\n%s = %s%s"  st.repr fld st'.repr suf
              ; params = st.params @ st'.params
              ; pos = st'.pos
              } in
            (st, i - 1) in
      let len = UpdateVec.vector_length updates in
      fst @@ UpdateVec.vector_fold_left
        { UpdateVec.f = fold }
        ({ blank_step with pos = st.pos }, len)
        updates

    let join_exprs ~handover st =
     E.Vec.vector_fold_left
       { E.Vec.f = fun (st, i) e ->
         let st' = E.build ~handover st e in
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

    let build_order_by ~handover st = function
      | Some exprs ->
          let st = fst (join_exprs ~handover st exprs) in
          { st with repr = "ORDER BY " ^ st.repr }
      | None ->
          st

    let seal ~handover (U { table; updates; where; limit; order_by }) =
      let st = { blank_step with pos = 1 } in
      let updates_st = build_updates ~handover st table updates in
      let where_st = build_where ~handover updates_st where in
      let order_by_st = build_order_by ~handover where_st order_by in
      let limit_st = build_limit order_by_st limit in
      let s =
        sprintf "UPDATE %s SET%s"
          (Table.to_string table)
          updates_st.repr
          in
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
end
