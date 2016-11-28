(** Sequoia MySQL driver *)

open Printf
open Sequoia_common

module D = struct let placeholder _ = "?" end
module M = Sequoia.Make (D)

include (M : module type of M
  with module Lit    := M.Lit
   and module Expr   := M.Expr
   and module Select := M.Select
   and module Update := M.Update
   and module Delete := M.Delete
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

module Enum = struct
  module type Instance = sig
    type t
    val to_string : string
  end

  module type S = sig
    type t
    val instance : t -> (module Instance)
  end
end

module Field = struct
  include (M.Field : module type of M.Field
    with module Null := M.Field.Null)

  type ('t, 'a) t +=
    | Time : string * 't M.Table.t -> ('t, [`Time] time) t
    | Timestamp : string * 't M.Table.t -> ('t, [`Timestamp] time) t
    | Date : string * 't M.Table.t -> ('t, [`Date] time) t
    | Datetime : string * 't M.Table.t -> ('t, [`Datetime] time) t
    | Enum : string * 't M.Table.t * (module Enum.Instance) -> ('t, (module Enum.Instance)) t

  module Null = struct
    include M.Field.Null

    type ('t, 'a) t +=
      | Time : string * 't M.Table.t -> ('t, [`Time] time option) t
      | Timestamp : string * 't M.Table.t -> ('t, [`Timestamp] time option) t
      | Date : string * 't M.Table.t -> ('t, [`Date] time option) t
      | Datetime : string * 't M.Table.t -> ('t, [`Datetime] time option) t
      | Enum : string * 't M.Table.t * (module Enum.Instance) -> ('t, (module Enum.Instance) option) t

    let time table name = Time (name, table)
    let timestamp table name = Timestamp (name, table)
    let date table name = Date (name, table)
    let datetime table name = Datetime (name, table)

    let enum table (module E : Enum.S) name =
      Enum (name, table, (module struct
        type t = E.t
        let to_string = "" (* dummy *)
      end : Enum.Instance))
  end

  let name : type a. ('t, a) t -> string = function
    | Time (n, _) -> n
    | Timestamp (n, _) -> n
    | Date (n, _) -> n
    | Datetime (n, _) -> n
    | Enum (n, _, _) -> n
    | Null.Time (n, _) -> n
    | Null.Timestamp (n, _) -> n
    | Null.Date (n, _) -> n
    | Null.Datetime (n, _) -> n
    | Null.Enum (n, _, _) -> n
    | fld -> name fld

  let table : type u a. (u, a) t -> u Table.t = function
    | Time (_, t) -> t
    | Timestamp (_, t) -> t
    | Date (_, t) -> t
    | Datetime (_, t) -> t
    | Enum (_, t, _) -> t
    | Null.Time (_, t) -> t
    | Null.Timestamp (_, t) -> t
    | Null.Date (_, t) -> t
    | Null.Datetime (_, t) -> t
    | Null.Enum (_, t, _) -> t
    | fld -> table fld

  let to_string : type a b. (a, b) t -> string = function
    | Time (name, table) -> sprintf "%s.%s" (Table.name table) name
    | Timestamp (name, table) -> sprintf "%s.%s" (Table.name table) name
    | Date (name, table) -> sprintf "%s.%s" (Table.name table) name
    | Datetime (name, table) -> sprintf "%s.%s" (Table.name table) name
    | Enum (name, table, _) -> sprintf "%s.%s" (Table.name table) name
    | Null.Time (name, table) -> sprintf "%s.%s" (Table.name table) name
    | Null.Timestamp (name, table) -> sprintf "%s.%s" (Table.name table) name
    | Null.Date (name, table) -> sprintf "%s.%s" (Table.name table) name
    | Null.Datetime (name, table) -> sprintf "%s.%s" (Table.name table) name
    | Null.Enum (name, table, _) -> sprintf "%s.%s" (Table.name table) name
    | other -> to_string other

  let to_string fld =
    let t = table fld in
    sprintf "%s.%s" (Table.name t) (name fld)

  let time table name = Time (name, table)
  let timestamp table name = Timestamp (name, table)
  let date table name = Date (name, table)
  let datetime table name = Datetime (name, table)

  let enum table (module E : Enum.S) name =
    Enum (name, table, (module struct
      type t = E.t
      let to_string = "" (* dummy *)
    end : Enum.Instance))
end

module type MYSQL_NULL_FIELD = sig
  include NULL_FIELD

  val time : string -> [`Time] time option t
  val timestamp : string -> [`Timestamp] time option t
  val date : string -> [`Date] time option t
  val datetime : string -> [`Datetime] time option t
  val enum : (module Enum.S) -> string -> (module Enum.Instance) option t
end

module type MYSQL_FIELD = sig
  include FIELD

  val time : string -> [`Time] time t
  val timestamp : string -> [`Timestamp] time t
  val date : string -> [`Date] time t
  val datetime : string -> [`Datetime] time t
  val enum : (module Enum.S) -> string -> (module Enum.Instance) t
end

module type MYSQL_TABLE = sig
  type t
  val table : t Table.t

  module Field : sig
    include MYSQL_FIELD with type table = t
    module Null : MYSQL_NULL_FIELD with type 'a t := 'a t
  end
end

module MakeMysqlTable (T: Sequoia.NAMED) : MYSQL_TABLE = struct
  module Base = MakeTable(T)
  include (Base : TABLE with type t := Base.t and module Field := Base.Field)
  type t = Base.t

  module Field = struct
    include (Base.Field : module type of Base.Field with
      module Null := Base.Field.Null)

    let time = Field.time table
    let timestamp = Field.timestamp table
    let date = Field.date table
    let datetime = Field.datetime table
    let enum = Field.enum table

    module Null = struct
      include Base.Field.Null

      let time = Field.Null.time table
      let timestamp = Field.Null.timestamp table
      let date = Field.Null.date table
      let datetime = Field.Null.datetime table
      let enum = Field.Null.enum table
    end
  end
end

let table name =
  (module struct
    include MakeMysqlTable (struct type t let name = name end)
  end : MYSQL_TABLE)

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

module Param = struct
  include M.Param

  type t +=
    | Time of [`Time] time
    | Timestamp of [`Timestamp] time
    | Date of [`Date] time
    | Datetime of [`Datetime] time
    | Enum of (module Enum.Instance)
end

module Lit = struct
  include M.Lit

  type 'a t +=
    | Time : [`Time] time -> [`Time] time t
    | Timestamp : [`Timestamp] time -> [`Timestamp] time t
    | Date : [`Date] time -> [`Date] time t
    | Datetime : [`Datetime] time -> [`Datetime] time t
    | Enum : (module Enum.Instance) -> (module Enum.Instance) t

  let to_param : type a. a t -> Param.t = function
    | Time t -> Param.Time t
    | Timestamp t -> Param.Timestamp t
    | Date d -> Param.Date d
    | Datetime d -> Param.Datetime d
    | Enum e -> Param.Enum e
    | lit -> to_param lit

  let build : type a. build_step -> a t -> build_step =
    fun st lit ->
      build_param D.placeholder st (to_param lit)
end

module Expr = struct
  include M.Expr
  include Base

  type 'a cast +=
    | Time : [`Time] time cast
    | Timestamp : [`Timestamp] time cast
    | Date : [`Date] time cast
    | Datetime : [`Datetime] time cast

  let string_of_cast : type a. a cast -> string = function
    | Time -> "TIME"
    | Timestamp -> "TIMESTAMP"
    | Date -> "DATE"
    | Datetime -> "DATETIME"
    | c -> string_of_cast c

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
    | Date_add : 'k time t * int * time_unit -> 'k t
    | Date_format : 'k time t * string -> string t
    | Date_sub : 'k time t * int * time_unit -> 'k time t
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

  let enum inst = fun _ ->
    Lit (Lit.Enum inst)

  let acos f = fun src -> Acos (f src)
  let ascii f = fun src -> Ascii (f src)
  let asin f = fun src -> Asin (f src)
  let atan f = fun src -> Atan (f src)
  let atan2 f g = fun src -> Atan2 (f src, g src)
  let avg f = fun src -> Avg (f src)
  let between f g = fun src -> Between (f src, g src)
  let bin f = fun src -> Bin (f src)
  let bit_and f = fun src -> Bit_and (f src)
  let bit_count f = fun src -> Bit_count (f src)
  let bit_length f = fun src -> Bit_length (f src)
  let bit_or f = fun src -> Bit_or (f src)
  let bit_xor f = fun src -> Bit_xor (f src)
  let ceil f = fun src -> Ceil (f src)
  let char f = fun src -> Char (f src)
  let char_length f = fun src -> Char_length (f src)
  let charset f = fun src -> Charset (f src)
  let coalesce f = fun src -> Coalesce (f src)
  let collation f = fun src -> Collation (f src)
  let compress f = fun src -> Compress (f src)
  let concat f = fun src -> Concat (f src)
  let concat_ws sep l = fun src -> Concat_ws (sep, List.map (fun f -> f src) l)
  let connection_id () = fun src -> Connection_id
  let conv f i j = fun src -> Conv (f src, i, j)
  let cos f = fun src -> Cos (f src)
  let cot f = fun src -> Cot (f src)
  let count ?(distinct = false) f = fun src -> Count (f src, distinct)
  let crc32 f = fun src -> Crc32 (f src)
  let current_date () = fun src -> Current_date
  let current_time () = fun src -> Current_time
  let current_timestamp () = fun src -> Current_timestamp
  let current_user () = fun src -> Current_user
  let database () = fun src -> Database
  let date_of : ('s M.Select.source -> [< `Date | `Datetime] time t) -> 's M.Select.source -> [`Date] time t = fun f src -> DateFn (f src)
  let date_add f i u = fun src -> Date_add (f src, i, u)
  let date_format f fmt = fun src -> Date_format (f src, fmt)
  let date_sub f i u = fun src -> Date_sub (f src, i, u)
  let datediff f g = fun src -> Datediff (f src, g src)
  let dayname f = fun src -> Dayname (f src)
  let dayofmonth f = fun src -> Dayofmonth (f src)
  let dayofweek f = fun src -> Dayofweek (f src)
  let dayofyear f = fun src -> Dayofyear (f src)
  let degrees f = fun src -> Degrees (f src)
  let expr f = fun src -> Expr (f src)
  let floor f = fun src -> Floor (f src)
  let from_days f = fun src -> From_days (f src)
  let from_unixtime f = fun src -> From_unixtime (f src)
  let if_ f g h = fun src -> If (f src, g src, h src)
  let ifnull f g = fun src -> Ifnull (f src, g src)
  let last_day f = fun src -> Last_day (f src)
  let length f = fun src -> Length (f src)
  let localtime () = fun src -> Localtime
  let log f = fun src -> Log (f src)
  let log10 f = fun src -> Log10 (f src)
  let log2 f = fun src -> Log2 (f src)
  let lower f = fun src -> Lower (f src)
  let lpad f = fun src -> Lpad (f src)
  let ltrim f = fun src -> Ltrim (f src)
  let max f = fun src -> Max (f src)
  let md5 f = fun src -> Md5 (f src)
  let minute f = fun src -> Minute (f src)
  let month f = fun src -> Month (f src)
  let monthname f = fun src -> Monthname (f src)
  let now () = fun src -> Now
  let nullif f g = fun src -> Nullif (f src, g src)
  let ord f = fun src -> Ord (f src)
  let pow f = fun src -> Pow (f src)
  let radians f = fun src -> Radians (f src)
  let rand f = fun src -> Rand (f src)
  let repeat f n = fun src -> Repeat (f src, n)
  let replace f s1 s2 = fun src -> Replace (f src, s1, s2)
  let reverse f = fun src -> Reverse (f src)
  let (=~) f re = fun src -> Rlike (f src, re)
  let round f = fun src -> Round (f src)
  let rpad f = fun src -> Rpad (f src)
  let rtrim f = fun src -> Rtrim (f src)
  let sec_to_time f = fun src -> Sec_to_time (f src)
  let second f = fun src -> Second (f src)
  let sha1 f = fun src -> Sha1 (f src)
  let sha2 f = fun src -> Sha2 (f src)
  let sign f = fun src -> Sign (f src)
  let sin f = fun src -> Sin (f src)
  let sqrt f = fun src -> Sqrt (f src)
  let stddev f = fun src -> Stddev (f src)
  let substring f pos len = fun src -> Substring (f src, pos, len)
  let substring_index f d n = fun src -> Substring_index (f src, d, n)
  let sum f d = fun src -> Sum (f src, d)
  let tan f = fun src -> Tan (f src)
  let time_of : ('s M.Select.source -> [< `Time | `Datetime] time t) -> 's M.Select.source -> [`Time] time t = fun f src -> TimeFn (f src)
  let trim f = fun src -> Trim (f src)
  let uncompress f = fun src -> Uncompress (f src)
  let utc_date () = fun src -> Utc_date
  let utc_time () = fun src -> Utc_time
  let utc_timestamp () = fun src -> Utc_timestamp
  let uuid () = fun src -> Uuid
  let uuid_short () = fun src -> Uuid_short
  let week () = fun src -> Week
  let weekday () = fun src -> Weekday
  let weekofyear () = fun src -> Weekofyear
  let upper f = fun src -> Upper (f src)
  let year () = fun src -> Year

  let as_time f = fun src -> Cast (f src, Time)
  let as_timestamp f = fun src -> Cast (f src, Timestamp)
  let as_date f = fun src -> Cast (f src, Date)
  let as_datetime f = fun src -> Cast (f src, Datetime)

  let rec build
    : type a. handover:handover -> build_step -> a t -> build_step =
    fun ~handover st e ->
      let build_param = build_param D.placeholder in
      let fn ?(st = st) =
        M.Expr.build_function ~placeholder:D.placeholder ~handover st in
      let open Base in
      match e with
      (* Data types *)
      | Lit (Lit.Time t) -> build_param st (Param.Time t)
      | Lit (Lit.Timestamp t) -> build_param st (Param.Timestamp t)
      | Lit (Lit.Date d) -> build_param st (Param.Date d)
      | Lit (Lit.Datetime d) -> build_param st (Param.Datetime d)
      | Lit (Lit.Enum e) ->
          build_param st (Param.Enum e)
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
          let s = string_of_time_unit u in
          fn "DATE_ADD(" [e] (sprintf ", INTERVAL %d %s)" i s)
      | Date_format (e, fmt) ->
          fn "DATE_FORMAT(" [e] (sprintf ", %s)" fmt)
      | Date_sub (e, i, u) ->
          let s = string_of_time_unit u in
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
      | Cast (e, Time) ->
          fn "CAST(" [e] (sprintf " AS %s)" (string_of_cast Time))
      | Cast (e, Timestamp) ->
          fn "CAST(" [e] (sprintf " AS %s)" (string_of_cast Timestamp))
      | Cast (e, Date) ->
          fn "CAST(" [e] (sprintf " AS %s)" (string_of_cast Date))
      | Cast (e, Datetime) ->
          fn "CAST(" [e] (sprintf " AS %s)" (string_of_cast Datetime))
      | e -> handover.expr st e
end

module Select = struct
  include (M.Select : module type of M.Select
    with module Expr := M.Select.Expr)

  let expr_build st e = Expr.build st e
  let cast_handover = Expr.string_of_cast

  module Expr = struct
    include M.Select.Expr

    let unwrap
      : type a. ('t, a option) Field.t -> ('b, 'c, 't, 'd) M.Select.steps
             -> 'b M.Select.source -> a t =
      fun fld sts src ->
        match fld with
        | Field.Null.Time (n, t) -> Field (Field.Time (n, t), src, sts)
        | Field.Null.Timestamp (n, t) -> Field (Field.Timestamp (n, t), src, sts)
        | Field.Null.Date (n, t) -> Field (Field.Date (n, t), src, sts)
        | Field.Null.Datetime (n, t) -> Field (Field.Datetime (n, t), src, sts)
        | Field.Null.Enum (n, t, e) -> Field (Field.Enum (n, t, e), src, sts)
        | _ -> unwrap fld sts src

    let build
      : type a. handover:Expr.handover
             -> build_step
             -> a Expr.t
             -> build_step =
      fun ~handover st -> function
        (* Fields *)
        | Field (Field.Time _ as fld, _, _) ->
            { st with repr = Field.to_string fld; pos = st.pos; params = [] }
        | Field (Field.Timestamp _ as fld, _, _) ->
            { st with repr = Field.to_string fld; pos = st.pos; params = [] }
        | Field (Field.Date _ as fld, _, _) ->
            { st with repr = Field.to_string fld; pos = st.pos; params = [] }
        | Field (Field.Datetime _ as fld, _, _) ->
            { st with repr = Field.to_string fld; pos = st.pos; params = [] }
        | Field (Field.Enum _ as fld, _, _) ->
            { st with repr = Field.to_string fld; pos = st.pos; params = [] }
        (* Handover *)
        | e -> build ~handover st e
  end

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
