type ('t, 'a) t = ..
type ('t, 'a) t +=
  | Bool : string * 't Sequoia_table.t -> ('t, bool) t
  | Int : string * 't Sequoia_table.t -> ('t, int) t
  | Float : string * 't Sequoia_table.t -> ('t, float) t
  | String : string * 't Sequoia_table.t -> ('t, string) t
  | Blob : string * 't Sequoia_table.t -> ('t, bytes) t

module Null : sig
  type ('t, 'a) t +=
    | Bool : string * 't Sequoia_table.t -> ('t, bool option) t
    | Int : string * 't Sequoia_table.t -> ('t, int option) t
    | Float : string * 't Sequoia_table.t -> ('t, float option) t
    | String : string * 't Sequoia_table.t -> ('t, string option) t
    | Blob : string * 't Sequoia_table.t -> ('t, bytes option) t

  val bool : 't Sequoia_table.t -> string -> ('t, bool option) t
  val int : 't Sequoia_table.t -> string -> ('t, int option) t
  val float : 't Sequoia_table.t -> string -> ('t, float option) t
  val string : 't Sequoia_table.t -> string -> ('t, string option) t
  val blob : 't Sequoia_table.t -> string -> ('t, bytes option) t
end

type ('t1, 't2) foreign_key = ('t1, int) t * ('t2, int) t

val name : ('t, 'a) t -> string
val table : ('t, 'a) t -> 't Sequoia_table.t
val to_string : ('t, 'a) t -> string

val bool : 't Sequoia_table.t -> string -> ('t, bool) t
val int : 't Sequoia_table.t -> string -> ('t, int) t
val float : 't Sequoia_table.t -> string -> ('t, float) t
val string : 't Sequoia_table.t -> string -> ('t, string) t
val blob : 't Sequoia_table.t -> string -> ('t, bytes) t

val foreign_key : 't1 Sequoia_table.t -> string -> references:('t2, int) t
               -> ('t1, 't2) foreign_key
