open Printf

module D = struct let placeholder _ = "?" end
module M = Sequoia.Make (D)

include (M : module type of M
  with module Lit    := M.Lit
   and module Expr   := M.Expr
   and module Select := M.Select
   and module Field  := M.Field
   and module Param  := M.Param)

type time_kind = [`Time | `Timestamp | `Date | `Datetime]
type time_fields =
  { year : int
  ; month : int
  ; day : int
  ; hour : int
  ; minute : int
  ; second : int
  }
type 'k time = time_fields constraint 'k = [< time_kind]

let base_time =
  { year = 0
  ; month = 0
  ; day = 0
  ; hour = 0
  ; minute = 0
  ; second = 0
  }

module Param = struct
  include M.Param

  type t +=
    | Time of [`Time] time
    | Timestamp of [`Timestamp] time
    | Date of [`Date] time
    | Datetime of [`Datetime] time
end

module Field = struct
  include (M.Field : module type of M.Field
    with module Null := M.Field.Null)

  type ('t, 'a) t +=
    | Time : string * 't M.Table.t -> ('t, [`Time] time) t
    | Timestamp : string * 't M.Table.t -> ('t, [`Timestamp] time) t
    | Date : string * 't M.Table.t -> ('t, [`Date] time) t
    | Datetime : string * 't M.Table.t -> ('t, [`Datetime] time) t

  module Null = struct
    include M.Field.Null

    type ('t, 'a) t +=
      | Time : string * 't M.Table.t -> ('t, [`Time] time option) t
      | Timestamp : string * 't M.Table.t -> ('t, [`Timestamp] time option) t
      | Date : string * 't M.Table.t -> ('t, [`Date] time option) t
      | Datetime : string * 't M.Table.t -> ('t, [`Datetime] time option) t

    let time table name = Time (name, table)
    let timestamp table name = Timestamp (name, table)
    let date table name = Date (name, table)
    let datetime table name = Datetime (name, table)
  end

  let to_string : type a b. (a, b) t -> string = function
    | Time (name, table) -> sprintf "%s.%s" table.Table.name name
    | Timestamp (name, table) -> sprintf "%s.%s" table.Table.name name
    | Date (name, table) -> sprintf "%s.%s" table.Table.name name
    | Datetime (name, table) -> sprintf "%s.%s" table.Table.name name
    | Null.Time (name, table) -> sprintf "%s.%s" table.Table.name name
    | Null.Timestamp (name, table) -> sprintf "%s.%s" table.Table.name name
    | Null.Date (name, table) -> sprintf "%s.%s" table.Table.name name
    | Null.Datetime (name, table) -> sprintf "%s.%s" table.Table.name name
    | other -> to_string other

  let time table name = Time (name, table)
  let timestamp table name = Timestamp (name, table)
  let date table name = Date (name, table)
  let datetime table name = Datetime (name, table)
end

module Lit = struct
  include M.Lit

  type time_unit
    = Seconds
    | Minutes
    | Hours
    | Days
    | Weeks
    | Months
    | Quarters
    | Years

  let string_of_time_unit = function
    | Seconds -> "SECOND"
    | Minutes -> "MINUTE"
    | Hours -> "HOUR"
    | Days -> "DAY"
    | Weeks -> "WEEK"
    | Months -> "MONTH"
    | Quarters -> "QUARTER"
    | Years -> "YEAR"

  type 'a t +=
    | Time : [`Time] time -> [`Time] time t
    | Timestamp : [`Timestamp] time -> [`Timestamp] time t
    | Date : [`Date] time -> [`Date] time t
    | Datetime : [`Datetime] time -> [`Datetime] time t
end

module Expr = struct
  include M.Expr

  type 'a t +=
    | Abs : int t -> int t
    | Acos : float t -> float t
    | Ascii : string t -> string t
    | Asin : float t -> float t
    | Atan : float t -> float t
    | Atan2 : float t * float t -> float t
    | Avg : float t -> float t
    | Between : 'a t * 'a t -> bool t
    | Bin : int t -> string t
    | Bit_and : int t -> int t
    | Bit_count : int t -> int t
    | Bit_length : string t -> int t
    | Bit_or : int t -> int t
    | Bit_xor : int t -> int t
    | Ceil : float t -> int t
    | Char : int t list -> string t
    | Char_length : string t -> int t
    | Charset : string t -> string t
    | Coalesce : 'a t list -> 'a t
    | Collation : string t -> string t
    | Compress : string t -> string t
    | Concat : string t list -> string t
    | Concat_ws : string * string t list -> string t
    | Connection_id : int t
    | Conv : int t * int * int -> string t
    | Cos : float t -> float t
    | Cot : float t -> float t
    | Count : 'a t * bool -> int t
    | Crc32 : string t -> int t
    | Current_date : [`Date] time t
    | Current_time : [`Time] time t
    | Current_timestamp : [`Timestamp] time t
    | Current_user : string t
    | Database : string t
    | DateFn : [< `Date | `Datetime] time t -> [`Date] time t
    | Date_add : 'k time t * int * Lit.time_unit -> 'k t
    | Date_format : 'k time t * string -> string t
    | Date_sub : 'k time t * int * Lit.time_unit -> 'k time t
    | Datediff : 'k time t * 'k time t -> int t
    | Dayname : 'k t -> string t
    | Dayofmonth : 'k t -> int t
    | Dayofweek : 'k t -> int t
    | Dayofyear : 'k t -> int t
    | Degrees : float t -> int t
    | Expr : float t -> float t
    | Floor : float t -> int t
    | From_days : int t -> [`Date] time t
    | From_unixtime : int t -> [`Datetime] time t
    | If : bool t * 'a t * 'a t -> 'a t
    | Ifnull : 'a option t * 'a t -> 'a t
    | Last_day : [< `Date | `Datetime] time t -> [`Date] time t
    | Length : string t -> int t
    | Localtime : [`Time] time t
    | Log : float t -> float t
    | Log10 : float t -> float t
    | Log2 : float t -> float t
    | Lower : string t -> string t
    | Lpad : string t -> string t
    | Ltrim : string t -> string t
    | Max : 'a t -> 'a t
    | Md5 : string t -> string t
    | Minute : string t -> string t
    | Month : 'k time t -> int t
    | Monthname : 'k time t -> string t
    | Now : [`Time] time t
    | Nullif : 'a t * 'a t -> 'a option t
    | Ord : string t -> int t
    | Pow : int t -> int t
    | Radians : float t -> float t
    | Rand : int t option -> float t
    | Repeat : string t * int -> string t
    | Replace : string t * string * string -> string t
    | Reverse : string t -> string t
    | Rlike : 'a t * string -> bool t
    | Round : float t -> int t
    | Rpad : string t -> string t
    | Rtrim : string t -> string t
    | Sec_to_time : int t -> string t
    | Second : string t -> int t
    | Sha1 : string t -> string t
    | Sha2 : string t -> string t
    | Sign : int t -> int t
    | Sin : float t -> float t
    | Sqrt : float t -> float t
    | Stddev : float t -> float t
    | Substring : string t * int * int option -> string t
    | Substring_index : string t * string * int -> string t
    | Sum : int t * bool -> int t
    | Tan : float t -> float t
    | TimeFn : [< `Time | `Datetime] time t -> [`Time] time t
    | Trim : string t -> string t
    | Uncompress : string t -> string t
    | Utc_date : string t
    | Utc_time : string t
    | Utc_timestamp : string t
    | Uuid : string t
    | Uuid_short : int t
    | Week : int t
    | Weekday : int t
    | Weekofyear : int t
    | Upper : string t -> string t
    | Year : int t
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
        (* Data types *)
        | Lit (Lit.Time t) -> build_param st (Param.Time t)
        | Lit (Lit.Timestamp t) -> build_param st (Param.Timestamp t)
        | Lit (Lit.Date d) -> build_param st (Param.Date d)
        | Lit (Lit.Datetime d) -> build_param st (Param.Datetime d)
        (* Functions *)
        | Abs e -> fn "ABS(" [e] ")"
        | Acos e -> fn "ACOS(" [e] ")"
        | Ascii e -> fn "ASCII(" [e] ")"
        | Asin e -> fn "ASIN(" [e] ")"
        | Atan e -> fn "ATAN(" [e] ")"
        | Atan2 (e1, e2) -> fn "ATAN2(" [e1; e2] ")"
        | Avg e -> fn "AVG(" [e] ")"
        | Between (e1, e2) -> fn "BETWEEN(" [e1; e2] ")"
        | Bin e -> fn "BIN(" [e] ")"
        | Bit_and e -> fn "BIT_AND" [e] ")"
        | Bit_count e -> fn "BIT_COUNT" [e] ")"
        | Bit_length e -> fn "BIT_LENGTH" [e] ")"
        | Bit_or e -> fn "BIT_OR" [e] ")"
        | Bit_xor e -> fn "BIT_XOR" [e] ")"
        | Ceil e -> fn "CEIL(" [e] ")"
        | Char l -> fn "CHAR(" l ")"
        | Char_length e -> fn "CHAR_LENGTH" [e] ")"
        | Charset e -> fn "CHARSET(" [e] ")"
        | Coalesce l -> fn "COALESCE(" l ")"
        | Collation e -> fn "COLLATION(" [e] ")"
        | Compress e -> fn "COMPRESS(" [e] ")"
        | Concat l -> fn "CONCAT(" l ")"
        | Concat_ws (s, l) -> fn "CONCAT_WS" l ")"
        | Connection_id -> fn "CONNECTION_ID" [] ")"
        | Conv (e1, i, j) -> fn "CONV(" [e1] (sprintf ", %d, %d)" i j)
        | Cos e -> fn "COS(" [e] ")"
        | Cot e -> fn "COT(" [e] ")"
        | Count (e, true) -> fn "COUNT(DISTINCT " [e] ")"
        | Count (e, false) -> fn "COUNT(" [e] ")"
        | Crc32 e -> fn "CRC32" [e] ")"
        | Current_date -> fn "CURRENT_DATE" [] ")"
        | Current_time -> fn "CURRENT_TIME" [] ")"
        | Current_timestamp -> fn "CURRENT_TIMESTAMP" [] ")"
        | Current_user -> fn "CURRENT_USER" [] ")"
        | Database -> fn "DATABASE(" [] ")"
        | DateFn e -> fn "DATE(" [e] ")"
        | Date_add (e, i, u) ->
            let s = Lit.string_of_time_unit u in
            fn "DATE_ADD(" [e] (sprintf ", INTERVAL %d %s)" i s)
        | Date_format (e, fmt) ->
            fn "DATE_FORMAT(" [e] (sprintf ", %s)" fmt)
        | Date_sub (e, i, u) ->
            let s = Lit.string_of_time_unit u in
            fn "DATE_SUB(" [e] (sprintf ", INTERVAL %d %s)" i s)
        | Datediff (e1, e2) -> fn "DATEDIFF(" [e1; e2] ")"
        | Dayname e -> fn "DAYNAME(" [e] ")"
        | Dayofmonth e -> fn "DAYOFMONTH(" [e] ")"
        | Dayofweek e -> fn "DAYOFWEEK(" [e] ")"
        | Dayofyear e -> fn "DAYOFYEAR(" [e] ")"
        | Degrees e -> fn "DEGREES(" [e] ")"
        | Expr e -> fn "EXPR(" [e] ")"
        | Floor e -> fn "FLOOR(" [e] ")"
        | If (b, t, f) ->
            let st1 = fn "IF(" [b] "" in
            let st2 = fn ~st:st1 (st1.repr ^ ", ") [t; f] ")" in
            Sequoia.{ st2 with params = st1.params @ st2.params }
        | From_days e -> fn "FROM_DAYS(" [e] ")"
        | From_unixtime e -> fn "FROM_UNIXTIME(" [e] ")"
        | Ifnull (e1, e2) ->
            let st1 = fn "IFNULL(" [e1] "" in
            let st2 = fn ~st:st1 (st1.repr ^ ", ") [e2] ")" in
            Sequoia.{ st2 with params = st1.params @ st2.params }
        | Last_day e -> fn "LAST_DAY(" [e] ")"
        | Length e -> fn "LENGTH(" [e] ")"
        | Localtime -> fn "LOCALTIME(" [] ")"
        | Log e -> fn "LOG(" [e] ")"
        | Log10 e -> fn "LOG10" [e] ")"
        | Log2 e -> fn "LOG2" [e] ")"
        | Lower e -> fn "LOWER(" [e] ")"
        | Lpad e -> fn "LPAD(" [e] ")"
        | Ltrim e -> fn "LTRIM(" [e] ")"
        | Max e -> fn "MAX(" [e] ")"
        | Md5 e -> fn "MD5" [e] ")"
        | Minute e -> fn "MINUTE(" [e] ")"
        | Month e -> fn "MONTH(" [e] ")"
        | Monthname e -> fn "MONTHNAME(" [e] ")"
        | Now -> fn "NOW(" [] ")"
        | Nullif (e1, e2) -> fn "NULLIF(" [e1; e2] ")"
        | Ord e -> fn "ORD(" [e] ")"
        | Pow e -> fn "POW(" [e] ")"
        | Radians e -> fn "RADIANS(" [e] ")"
        | Rand (Some seed) -> fn "RAND(" [seed] ")"
        | Rand None -> fn "RAND(" [] ")"
        | Repeat (e, n) -> fn "REPEAT(" [e] (sprintf ", %d)" n)
        | Replace (e, s1, s2) -> fn "REPLACE(" [e] (sprintf ", %s, %s)" s1 s2)
        | Reverse e -> fn "REVERSE(" [e] ")"
        | Rlike (e, re) -> fn "RLIKE(" [e] (sprintf ", %s)" re)
        | Round e -> fn "ROUND(" [e] ")"
        | Rpad e -> fn "RPAD(" [e] ")"
        | Rtrim e -> fn "RTRIM(" [e] ")"
        | Sec_to_time e -> fn "SEC_TO_TIME" [e] ")"
        | Second e -> fn "SECOND(" [e] ")"
        | Sha1 e -> fn "SHA1" [e] ")"
        | Sha2 e -> fn "SHA2" [e] ")"
        | Sign e -> fn "SIGN(" [e] ")"
        | Sin e -> fn "SIN(" [e] ")"
        | Sqrt e -> fn "SQRT(" [e] ")"
        | Stddev e -> fn "STDDEV(" [e] ")"
        | Substring (e, pos, Some len) ->
            fn "SUBSTRING(" [e] (sprintf " FROM %d FOR %d)" pos len)
        | Substring (e, pos, None) ->
            fn "SUBSTRING(" [e] (sprintf " FROM %d)" pos)
        | Substring_index (e, delim, n) ->
            fn "SUBSTRING_INDEX(" [e] (sprintf ", %s, %d)" delim n)
        | Sum (e, true) -> fn "SUM(DISTINCT " [e] ")"
        | Sum (e, false) -> fn "SUM(" [e] ")"
        | Tan e -> fn "TAN(" [e] ")"
        | TimeFn e -> fn "TIME(" [e] ")"
        | Trim e -> fn "TRIM(" [e] ")"
        | Uncompress e -> fn "UNCOMPRESS(" [e] ")"
        | e -> build ~handover st e

    let unwrap
      : type a. ('t, a option) Field.t -> ('b, 'c, 't, 'd) M.Select.steps
             -> 'b M.Select.source -> a t =
      fun fld sts src ->
        match fld with
        | Field.Null.Time (n, t) -> Field (Field.Time (n, t), src, sts)
        | Field.Null.Timestamp (n, t) -> Field (Field.Timestamp (n, t), src, sts)
        | Field.Null.Date (n, t) -> Field (Field.Date (n, t), src, sts)
        | Field.Null.Datetime (n, t) -> Field (Field.Datetime (n, t), src, sts)
        | _ -> unwrap fld sts src

    let time ?(hour = 0) ?(minute = 0) ?(second = 0) = fun _ ->
      Lit (Lit.Time { base_time with hour; minute; second })
    let timestamp t = fun _ ->
      let tm = Unix.localtime t in
      let ts = Lit.Timestamp
        { year = tm.Unix.tm_year
        ; month = tm.Unix.tm_mon
        ; day = tm.Unix.tm_mday
        ; hour = tm.Unix.tm_hour
        ; minute = tm.Unix.tm_min
        ; second = tm.Unix.tm_sec
        } in
      Lit ts
    let date ?(year = 0) ?(month = 0) ?(day = 0) = fun _ ->
      Lit (Lit.Date { base_time with year; month; day })
    let datetime ?(year = 0) ?(month = 0) ?(day = 0)
                 ?(hour = 0) ?(minute = 0) ?(second = 0) = fun _ ->
      Lit (Lit.Datetime { base_time with year; month; day; minute; second })

    let acos f = fun src -> Expr.Acos (f src)
    let ascii f = fun src -> Expr.Ascii (f src)
    let asin f = fun src -> Expr.Asin (f src)
    let atan f = fun src -> Expr.Atan (f src)
    let atan2 f g = fun src -> Expr.Atan2 (f src, g src)
    let avg f = fun src -> Expr.Avg (f src)
    let between f g = fun src -> Expr.Between (f src, g src)
    let bin f = fun src -> Expr.Bin (f src)
    let bit_and f = fun src -> Expr.Bit_and (f src)
    let bit_count f = fun src -> Expr.Bit_count (f src)
    let bit_length f = fun src -> Expr.Bit_length (f src)
    let bit_or f = fun src -> Expr.Bit_or (f src)
    let bit_xor f = fun src -> Expr.Bit_xor (f src)
    let ceil f = fun src -> Expr.Ceil (f src)
    let char f = fun src -> Expr.Char (f src)
    let char_length f = fun src -> Expr.Char_length (f src)
    let charset f = fun src -> Expr.Charset (f src)
    let coalesce f = fun src -> Expr.Coalesce (f src)
    let collation f = fun src -> Expr.Collation (f src)
    let compress f = fun src -> Expr.Compress (f src)
    let concat f = fun src -> Expr.Concat (f src)
    let concat_ws sep l = fun src -> Expr.Concat_ws (sep, List.map (fun f -> f src) l)
    let connection_id () = fun src -> Expr.Connection_id
    let conv f i j = fun src -> Expr.Conv (f src, i, j)
    let cos f = fun src -> Expr.Cos (f src)
    let cot f = fun src -> Expr.Cot (f src)
    let count ?(distinct = false) f = fun src -> Expr.Count (f src, distinct)
    let crc32 f = fun src -> Expr.Crc32 (f src)
    let current_date () = fun src -> Expr.Current_date
    let current_time () = fun src -> Expr.Current_time
    let current_timestamp () = fun src -> Expr.Current_timestamp
    let current_user () = fun src -> Expr.Current_user
    let database () = fun src -> Expr.Database
    let date_of : ('s M.Select.source -> [< `Date | `Datetime] time t) -> 's M.Select.source -> [`Date] time t = fun f src -> Expr.DateFn (f src)
    let date_add f i u = fun src -> Expr.Date_add (f src, i, u)
    let date_format f fmt = fun src -> Expr.Date_format (f src, fmt)
    let date_sub f i u = fun src -> Expr.Date_sub (f src, i, u)
    let datediff f g = fun src -> Expr.Datediff (f src, g src)
    let dayname f = fun src -> Expr.Dayname (f src)
    let dayofmonth f = fun src -> Expr.Dayofmonth (f src)
    let dayofweek f = fun src -> Expr.Dayofweek (f src)
    let dayofyear f = fun src -> Expr.Dayofyear (f src)
    let degrees f = fun src -> Expr.Degrees (f src)
    let expr f = fun src -> Expr.Expr (f src)
    let floor f = fun src -> Expr.Floor (f src)
    let from_days f = fun src -> Expr.From_days (f src)
    let from_unixtime f = fun src -> Expr.From_unixtime (f src)
    let if_ f g h = fun src -> Expr.If (f src, g src, h src)
    let ifnull f g = fun src -> Expr.Ifnull (f src, g src)
    let last_day f = fun src -> Expr.Last_day (f src)
    let length f = fun src -> Expr.Length (f src)
    let localtime () = fun src -> Expr.Localtime
    let log f = fun src -> Expr.Log (f src)
    let log10 f = fun src -> Expr.Log10 (f src)
    let log2 f = fun src -> Expr.Log2 (f src)
    let lower f = fun src -> Expr.Lower (f src)
    let lpad f = fun src -> Expr.Lpad (f src)
    let ltrim f = fun src -> Expr.Ltrim (f src)
    let max f = fun src -> Expr.Max (f src)
    let md5 f = fun src -> Expr.Md5 (f src)
    let minute f = fun src -> Expr.Minute (f src)
    let month f = fun src -> Expr.Month (f src)
    let monthname f = fun src -> Expr.Monthname (f src)
    let now () = fun src -> Expr.Now
    let nullif f g = fun src -> Expr.Nullif (f src, g src)
    let ord f = fun src -> Expr.Ord (f src)
    let pow f = fun src -> Expr.Pow (f src)
    let radians f = fun src -> Expr.Radians (f src)
    let rand f = fun src -> Expr.Rand (f src)
    let repeat f n = fun src -> Expr.Repeat (f src, n)
    let replace f s1 s2 = fun src -> Expr.Replace (f src, s1, s2)
    let reverse f = fun src -> Expr.Reverse (f src)
    let (=~) f re = fun src -> Expr.Rlike (f src, re)
    let round f = fun src -> Expr.Round (f src)
    let rpad f = fun src -> Expr.Rpad (f src)
    let rtrim f = fun src -> Expr.Rtrim (f src)
    let sec_to_time f = fun src -> Expr.Sec_to_time (f src)
    let second f = fun src -> Expr.Second (f src)
    let sha1 f = fun src -> Expr.Sha1 (f src)
    let sha2 f = fun src -> Expr.Sha2 (f src)
    let sign f = fun src -> Expr.Sign (f src)
    let sin f = fun src -> Expr.Sin (f src)
    let sqrt f = fun src -> Expr.Sqrt (f src)
    let stddev f = fun src -> Expr.Stddev (f src)
    let substring f pos len = fun src -> Expr.Substring (f src, pos, len)
    let substring_index f d n = fun src -> Expr.Substring_index (f src, d, n)
    let sum f d = fun src -> Expr.Sum (f src, d)
    let tan f = fun src -> Expr.Tan (f src)
    let time_of : ('s M.Select.source -> [< `Time | `Datetime] time t) -> 's M.Select.source -> [`Time] time t = fun f src -> Expr.TimeFn (f src)
    let trim f = fun src -> Expr.Trim (f src)
    let uncompress f = fun src -> Expr.Uncompress (f src)
    let utc_date () = fun src -> Expr.Utc_date
    let utc_time () = fun src -> Expr.Utc_time
    let utc_timestamp () = fun src -> Expr.Utc_timestamp
    let uuid () = fun src -> Expr.Uuid
    let uuid_short () = fun src -> Expr.Uuid_short
    let week () = fun src -> Expr.Week
    let weekday () = fun src -> Expr.Weekday
    let weekofyear () = fun src -> Expr.Weekofyear
    let upper f = fun src -> Expr.Upper (f src)
    let year () = fun src -> Expr.Year
  end

  let seal stmt = seal ~handover:{ Expr.handover = Expr.builder } stmt
end
