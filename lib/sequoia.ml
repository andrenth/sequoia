open Printf

let ident = 2

let psprintf ?(pad = 0) fmt =
  ksprintf (fun s -> sprintf "%*s%s" pad "" s) fmt

let table_to_string db col alias =
  match alias with
  | Some a -> sprintf "`%s`.`%s` AS `%s`" db col a
  | None -> sprintf "`%s`.`%s`" db col

let join = String.concat

module From = struct
  type t = string * string * string option

  let to_string (db, col, alias) =
    table_to_string db col alias
end

module Limit = struct
  type t = int option * int

  let to_string (off, lim) =
    match off with
    | Some off -> sprintf "(%d, %d)" off lim
    | None -> sprintf "%d" lim
end

module rec Expr : sig
  type t =
    [ `Int of int
    | `Float of float
    | `String of string
    | `Column of string list
    | `Alias of t * string
    | `Like of t * t
    | `Regexp of t * t
    | `Not of t
    | `In of t * t list
    | `Exists of Select.t
    | `Fun of Function.t
    | `List of t list
    | `Select of Select.t
    | `Null of Expr.t
    ]

  val to_string : t -> string
end = struct
  type t =
    [ `Int of int
    | `Float of float
    | `String of string
    | `Column of string list
    | `Alias of t * string
    | `Like of t * t
    | `Regexp of t * t
    | `Not of t
    | `In of t * t list
    | `Exists of Select.t
    | `Fun of Function.t
    | `List of t list
    | `Select of Select.t
    | `Null of Expr.t
    ]

  let column_to_string c =
    join "." (List.map (sprintf "`%s`") c)

  let maps f s =
    Array.init
      (String.length s)
      (fun i -> f s.[i])
    |> Array.fold_left (fun acc s -> acc ^ s) ""

  let escape = maps Char.escaped

  let rec to_string : t -> string = function
    | `Int i -> sprintf "%d" i
    | `Float x -> sprintf "%f" x
    | `String s -> sprintf "'%s'" (escape s)
    | `Column c -> sprintf "%s" (column_to_string c)
    | `Alias (e, a) -> sprintf "%s" (to_string e ^ " AS " ^ a)
    | `Like (e, p) -> sprintf "%s LIKE %s" (to_string e) (to_string p)
    | `Regexp (e, r) -> sprintf "%s REGEXP '%s'" (to_string e) (to_string r)
    | `Not (`Like (e, p)) ->
        sprintf "%s NOT LIKE %s" (to_string e) (to_string p)
    | `Not (`Regexp (e, p)) ->
        sprintf "%s NOT REGEXP %s" (to_string e) (to_string p)
    | `Not (`Null e) -> sprintf "IS NOT NULL %s" (to_string e)
    | `Not e -> sprintf "NOT %s" (to_string e)
    | `In (e, l) ->
        sprintf "%s IN (%s)" (to_string e) (join ", " (List.map to_string l))
    | `Exists sel -> sprintf "EXISTS (%s)" (Select.to_string sel)
    | `Fun f -> sprintf "%s" (Function.to_string f)
    | `List l -> sprintf "%s" (join ", " (List.map to_string l))
    | `Select s -> sprintf "(%s)" (Select.to_string s)
    | `Null e -> sprintf "IS NULL %s" (to_string e)
end

and Select : sig
  type t =
    { expr     : Expr.t
    ; from     : From.t option
    ; joins    : Join.t list
    ; where    : Expr.t option
    ; group_by : (Expr.t * Expr.t option) option
    ; order_by : Expr.t option
    ; limit    : Limit.t option
    }

  val to_string : ?pad:int -> t -> string
end = struct
  type t =
    { expr     : Expr.t
    ; from     : From.t option
    ; joins    : Join.t list
    ; where    : Expr.t option
    ; group_by : (Expr.t * Expr.t option) option
    ; order_by : Expr.t option
    ; limit    : Limit.t option
    }

  let clause_to_string name to_string clause =
    sprintf "%s %s" name (to_string clause)

  let clause_opt_to_string name to_string = function
    | Some c -> clause_to_string name to_string c
    | None -> ""

  let joins_to_string = function
    | [] -> ""
    | joins -> " " ^ join " " (List.map Join.to_string joins)

  let group_by_opt_to_string = function
    | Some (group_by, having_opt) ->
      let group_by_str =
        clause_to_string " GROUP BY" Expr.to_string group_by in
      let having_str =
        clause_opt_to_string " HAVING" Expr.to_string having_opt in
      group_by_str ^ having_str
    | None ->
        ""

  let to_string ?(pad = 0) s =
    sprintf "%s%s%s%s%s%s%s"
      (clause_to_string "SELECT" Expr.to_string s.expr)
      (clause_opt_to_string " FROM" From.to_string s.from)
      (joins_to_string s.joins)
      (clause_opt_to_string " WHERE" Expr.to_string s.where)
      (group_by_opt_to_string s.group_by)
      (clause_opt_to_string " ORDER BY" Expr.to_string s.order_by)
      (clause_opt_to_string " LIMIT" Limit.to_string s.limit)
end

and Join : sig
  type kind =
    [ `Left of bool
    | `Right of bool
    ]

  type cond =
    [ `On of Expr.t
    | `Using of string list
    ]

  type t = kind * string * string * string option * cond

  val to_string : t -> string
end = struct
  type kind =
    [ `Left of bool
    | `Right of bool
    ]

  type cond =
    [ `On of Expr.t
    | `Using of string list
    ]

  type t = kind * string * string * string option * cond

  let spec_to_string = function
    | `Left outer -> sprintf "LEFT %sJOIN" (if outer then "OUTER " else "")
    | `Right outer -> sprintf "RIGHT %sJOIN" (if outer then "OUTER " else "")

  let cond_to_string = function
    | `On expr -> "ON " ^ Expr.to_string expr
    | `Using cols -> "USING (" ^ join ", " cols ^ ")"

  let to_string (spec, db, col, alias, cond) =
    sprintf "%s %s %s"
      (spec_to_string spec)
      (table_to_string db col alias)
      (cond_to_string cond)
end

and Function : sig
  type t =
    [ `Eq of Expr.t * Expr.t
    | `Nulleq of Expr.t * Expr.t
    | `Ne of Expr.t * Expr.t
    | `Gt of Expr.t * Expr.t
    | `Lt of Expr.t * Expr.t
    | `Ge of Expr.t * Expr.t
    | `Le of Expr.t * Expr.t
    | `Add of Expr.t * Expr.t
    | `Sub of Expr.t * Expr.t
    | `Mul of Expr.t * Expr.t
    | `Div of Expr.t * Expr.t
    | `And of Expr.t * Expr.t
    | `Or of Expr.t * Expr.t
    | `Bit_and of Expr.t * Expr.t
    | `Bit_or of Expr.t * Expr.t
    | `Shift_left of Expr.t * Expr.t
    | `Shift_right of Expr.t * Expr.t
    | `Mod of Expr.t * Expr.t
    | `If of Expr.t * Expr.t * Expr.t
    | `Ifnull of Expr.t * Expr.t
    | `Curdate
    | `Current_date
    | `Curtime
    | `Current_time
    | `Current_timestamp
    | `Date_format of Expr.t * Expr.t
    | `Date of Expr.t
    | `Datediff of Expr.t * Expr.t
    | `Day of Expr.t
    | `Dayname of Expr.t
    | `Dayofmonth of Expr.t
    | `Dayofweek of Expr.t
    | `Dayofyear of Expr.t
    | `From_days of Expr.t
    | `From_unixtime of Expr.t
    | `Hour of Expr.t
    | `Localtime
    | `Localtimestamp
    | `Makedate of Expr.t * Expr.t
    | `Maketime of Expr.t * Expr.t * Expr.t
    | `Minute of Expr.t
    | `Month of Expr.t
    | `Monthname of Expr.t
    | `Now
    | `Period_add of Expr.t * Expr.t
    | `Period_diff of Expr.t * Expr.t
    | `Quarter of Expr.t
    | `Second of Expr.t
    | `Sec_to_time of Expr.t
    | `Str_to_date of Expr.t * Expr.t
    | `Subtime of Expr.t * Expr.t
    | `Sysdate of Expr.t * Expr.t
    | `Time of Expr.t
    | `Timestamp of Expr.t
    | `Time_format of Expr.t * Expr.t
    | `Time_to_sec of Expr.t
    | `To_days of Expr.t
    | `Unix_timestamp
    | `Utc_date
    | `Utc_timestamp
    | `Weekday of Expr.t
    | `Weekofyear of Expr.t
    | `Year of Expr.t
    | `Yearweek of Expr.t
    ]

  val to_string : t -> string
end = struct
  type t =
    [ `Eq of Expr.t * Expr.t
    | `Nulleq of Expr.t * Expr.t
    | `Ne of Expr.t * Expr.t
    | `Gt of Expr.t * Expr.t
    | `Lt of Expr.t * Expr.t
    | `Ge of Expr.t * Expr.t
    | `Le of Expr.t * Expr.t
    | `Add of Expr.t * Expr.t
    | `Sub of Expr.t * Expr.t
    | `Mul of Expr.t * Expr.t
    | `Div of Expr.t * Expr.t
    | `And of Expr.t * Expr.t
    | `Or of Expr.t * Expr.t
    | `Bit_and of Expr.t * Expr.t
    | `Bit_or of Expr.t * Expr.t
    | `Shift_left of Expr.t * Expr.t
    | `Shift_right of Expr.t * Expr.t
    | `Mod of Expr.t * Expr.t
    | `If of Expr.t * Expr.t * Expr.t
    | `Ifnull of Expr.t * Expr.t
    | `Curdate
    | `Current_date
    | `Curtime
    | `Current_time
    | `Current_timestamp
    | `Date_format of Expr.t * Expr.t
    | `Date of Expr.t
    | `Datediff of Expr.t * Expr.t
    | `Day of Expr.t
    | `Dayname of Expr.t
    | `Dayofmonth of Expr.t
    | `Dayofweek of Expr.t
    | `Dayofyear of Expr.t
    | `From_days of Expr.t
    | `From_unixtime of Expr.t
    | `Hour of Expr.t
    | `Localtime
    | `Localtimestamp
    | `Makedate of Expr.t * Expr.t
    | `Maketime of Expr.t * Expr.t * Expr.t
    | `Minute of Expr.t
    | `Month of Expr.t
    | `Monthname of Expr.t
    | `Now
    | `Period_add of Expr.t * Expr.t
    | `Period_diff of Expr.t * Expr.t
    | `Quarter of Expr.t
    | `Second of Expr.t
    | `Sec_to_time of Expr.t
    | `Str_to_date of Expr.t * Expr.t
    | `Subtime of Expr.t * Expr.t
    | `Sysdate of Expr.t * Expr.t
    | `Time of Expr.t
    | `Timestamp of Expr.t
    | `Time_format of Expr.t * Expr.t
    | `Time_to_sec of Expr.t
    | `To_days of Expr.t
    | `Unix_timestamp
    | `Utc_date
    | `Utc_timestamp
    | `Weekday of Expr.t
    | `Weekofyear of Expr.t
    | `Year of Expr.t
    | `Yearweek of Expr.t
    ]

  let to_string = function
    | `Eq (e1, e2) ->
        sprintf "(%s) = (%s)" (Expr.to_string e1) (Expr.to_string e2)
    | `Nulleq (e1, e2) ->
        sprintf "(%s) <=> (%s)" (Expr.to_string e1) (Expr.to_string e2)
    | `Ne (e1, e2) ->
        sprintf "(%s) != (%s)" (Expr.to_string e1) (Expr.to_string e2)
    | `Gt (e1, e2) ->
        sprintf "(%s) > (%s)" (Expr.to_string e1) (Expr.to_string e2)
    | `Lt (e1, e2) ->
        sprintf "(%s) < (%s)" (Expr.to_string e1) (Expr.to_string e2)
    | `Ge (e1, e2) ->
        sprintf "(%s) >= (%s)" (Expr.to_string e1) (Expr.to_string e2)
    | `Le (e1, e2) ->
        sprintf "(%s) <= (%s)" (Expr.to_string e1) (Expr.to_string e2)
    | `Add (e1, e2) ->
        sprintf "(%s) + (%s)" (Expr.to_string e1) (Expr.to_string e2)
    | `Sub (e1, e2) ->
        sprintf "(%s) - (%s)" (Expr.to_string e1) (Expr.to_string e2)
    | `Mul (e1, e2) ->
        sprintf "(%s) + (%s)" (Expr.to_string e1) (Expr.to_string e2)
    | `Div (e1, e2) ->
        sprintf "(%s) / (%s)" (Expr.to_string e1) (Expr.to_string e2)
    | `And (e1, e2) ->
        sprintf "(%s) AND (%s)" (Expr.to_string e1) (Expr.to_string e2)
    | `Or (e1, e2) ->
        sprintf "(%s) OR (%s)" (Expr.to_string e1) (Expr.to_string e2)
    | `Bit_and (e1, e2) ->
        sprintf "(%s) & (%s)" (Expr.to_string e1) (Expr.to_string e2)
    | `Bit_or (e1, e2) ->
        sprintf "(%s) | (%s)" (Expr.to_string e1) (Expr.to_string e2)
    | `Shift_left (e1, e2) ->
        sprintf "(%s) << (%s)" (Expr.to_string e1) (Expr.to_string e2)
    | `Shift_right (e1, e2) ->
        sprintf "(%s) >> (%s)" (Expr.to_string e1) (Expr.to_string e2)
    | `Mod (e1, e2) ->
        sprintf "(%s) %% (%s)" (Expr.to_string e1) (Expr.to_string e2)
    | `If (e1, e2, e3) ->
        sprintf "IF(%s, %s, %s)"
          (Expr.to_string e1) (Expr.to_string e2) (Expr.to_string e3)
    | `Ifnull (e1, e2) ->
        sprintf "IFNULL(%s, %s)" (Expr.to_string e1) (Expr.to_string e2)
    | `Curdate ->
        "CURDATE()"
    | `Current_date ->
        "CURRENT_DATE()"
    | `Curtime ->
        "CURTIME()"
    | `Current_time ->
        "CURRENT_TIME()"
    | `Current_timestamp ->
        "CURRENT_TIMESTAMP()"
    | `Date_format (e1, e2) ->
        sprintf "DATE_FORMAT(%s, %s)" (Expr.to_string e1) (Expr.to_string e2)
    | `Date e ->
        sprintf "DATE(%s)" (Expr.to_string e)
    | `Datediff (e1, e2) ->
        sprintf "DATEDIFF(%s, %s)" (Expr.to_string e1) (Expr.to_string e2)
    | `Day e ->
        sprintf "DAY(%s)" (Expr.to_string e)
    | `Dayname e ->
        sprintf "DAYNAME(%s)" (Expr.to_string e)
    | `Dayofmonth e ->
        sprintf "DAYOFMONTH(%s)" (Expr.to_string e)
    | `Dayofweek e ->
        sprintf "DAYOFWEEK(%s)" (Expr.to_string e)
    | `Dayofyear e ->
        sprintf "DAYOFYEAR(%s)" (Expr.to_string e)
    | `From_days e ->
        sprintf "FROM_DAYS(%s)" (Expr.to_string e)
    | `From_unixtime e ->
        sprintf "FROM_UNIXTIME(%s)" (Expr.to_string e)
    | `Hour e ->
        sprintf "HOUR(%s)" (Expr.to_string e)
    | `Localtime ->
        "LOCALTIME()"
    | `Localtimestamp ->
        "LOCALTIMESTAMP()"
    | `Makedate (e1, e2) ->
        sprintf "MAKEDATE(%s, %s)" (Expr.to_string e1) (Expr.to_string e2)
    | `Maketime (e1, e2, e3) ->
        sprintf "MAKETIME(%s, %s, %s)"
          (Expr.to_string e1) (Expr.to_string e2) (Expr.to_string e3)
    | `Minute e ->
        sprintf "MINUTE(%s)" (Expr.to_string e)
    | `Month e ->
        sprintf "MONTH(%s)" (Expr.to_string e)
    | `Monthname e ->
        sprintf "MONTHNAME(%s)" (Expr.to_string e)
    | `Now ->
        "NOW()"
    | `Period_add (e1, e2) ->
        sprintf "PERIOD_ADD(%s, %s)" (Expr.to_string e1) (Expr.to_string e2)
    | `Period_diff (e1, e2) ->
        sprintf "PERIOD_DIFF(%s, %s)" (Expr.to_string e1) (Expr.to_string e2)
    | `Quarter e ->
        sprintf "QUARTER(%s)" (Expr.to_string e)
    | `Second e ->
        sprintf "SECOND(%s)" (Expr.to_string e)
    | `Sec_to_time e ->
        sprintf "SEC_TO_TIME(%s)" (Expr.to_string e)
    | `Str_to_date (e1, e2) ->
        sprintf "STR_TO_DATE(%s, %s)" (Expr.to_string e1) (Expr.to_string e2)
    | `Subtime (e1, e2) ->
        sprintf "SUBTIME(%s, %s)" (Expr.to_string e1) (Expr.to_string e2)
    | `Sysdate (e1, e2) ->
        sprintf "SYSDATE(%s, %s)" (Expr.to_string e1) (Expr.to_string e2)
    | `Time e ->
        sprintf "TIME(%s)" (Expr.to_string e)
    | `Timestamp e ->
        sprintf "TIMESTAMP(%s)" (Expr.to_string e)
    | `Time_format (e1, e2) ->
        sprintf "TIME_FORMAT(%s, %s)" (Expr.to_string e1) (Expr.to_string e2)
    | `Time_to_sec e ->
        sprintf "TIME_TO_SEC(%s)" (Expr.to_string e)
    | `To_days e ->
        sprintf "TO_DAYS(%s)" (Expr.to_string e)
    | `Unix_timestamp ->
        "UNIX_TIMESTAMP()"
    | `Utc_date ->
        "UTC_DATE()"
    | `Utc_timestamp ->
        "UTC_TIMESTAMP()"
    | `Weekday e ->
        sprintf "WEEKDAY(%s)" (Expr.to_string e)
    | `Weekofyear e ->
        sprintf "WEEKOFYEAR(%s)" (Expr.to_string e)
    | `Year e ->
        sprintf "YEAR(%s)" (Expr.to_string e)
    | `Yearweek e ->
        sprintf "YEARWEEK(%s)" (Expr.to_string e)
end

let int i = `Int i
let float x = `Float x
let string s = `String s
let list_of f l = `List (List.map f l)

