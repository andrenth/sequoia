open Printf

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
  sprintf "%s.%s" (Table.name t) (name fld)

let foreign_key table name ~references = (Int (name, table), references)

let bool table name = Bool (name, table)
let int table name = Int (name, table)
let float table name = Float (name, table)
let string table name = String (name, table)
let blob table name = Blob (name, table)
