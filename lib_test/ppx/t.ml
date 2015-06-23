open Printf

module Db = struct
  type t1 =
    { id   : int    [@sql]
    ; c1   : string [@sql "column1"]
    ; c2   : string [@sql "c2"]
    } [@@sql "t1"]

  type t2 =
    { id    : int    [@sql "id"]
    ; t1_id : int    [@sql "t1_id"]
    ; c1    : string [@sql "c1"]
    ; c2    : string [@sql "c2"]
    } [@@sql "t2"]
end [@@sql "db"]

let set = Sequoia.(list_of int) [1; 2; 3]
let pat = Sequoia.string "foo%"
let x = Sequoia.int 42
(* A nonsense query to test most features *)
let%sql query =
  select
    ((select q.c1 from Db.t2 as_ q) as_ sub,
     foo.c1,
     now(),
     ifnull(Db.t1.c1, Db.t2.c2))
  from
    Db.t1 as_ foo
  left join
    Db.t2 on (Db.t2.t1_id = foo.id)
  left join
    Db.t2 as_ bar on (bar.t1_id = foo.id)
  where
    ((not exists (select 1))
      && (Db.t1.c1 is null || Db.t1.c1 = !x)
      && (Db.t2.c2 in_ !set || Db.t2.c2 in_ (10,20,30))
      && (Db.t2.c2 not like !pat))
  group by
    (select Db.t2.c1)
  having
    (foo.c1 = 1)
  order by
    Db.t2.c1
  limit (10, 20)

let () =
  printf "%s\n%!" (Sequoia.Select.to_string query)
