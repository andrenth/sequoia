# Sequoia

Sequoia is a safe query builder for MySQL. You can map a database using an
OCaml module like so:

```ocaml
module MyDB = struct
  type my_table = {
     id   : int    [@sql];
     name : string [@sql];
  } [@@sql]
end [@@sql]
```

The `@sql`/`@@sql` attributes can take an extra string that will map to the
real MySQL column/table/database names in case they differ from their
OCaml names.

With that definition you can then build a query:

```ocaml
let%sql query =
  select
    t.name
  from
    MyDB.my_table as_ t
```

Which will build the query below via `Sequoia.Select.to_string query`:

```sql
SELECT `t`.`name` FROM `MyDB`.`my_table` AS `t`
```

Variable substitution is supported using the ref syntax:

```ocaml
let pat = Sequoia.string "a%"
let%sql query =
  select
    t.name
  from
    MyDB.my_table as_ t
  where
    (t.name like !pat && t.id > 1)
```

If you reference a database, table or column that isn't declared via the
@sql/@@sql attributes or call a nonexistent MySQL function, you get a
compile-time error:

```ocaml
let%sql query =
  select
    (frob(t.name))
  from
    MyDB.my_table as_ t
```

    This expression has type [> `Frob of [> `Column of string list ] ]
    but an expression was expected of type Sequoia.Function.t
    The second variant type does not allow tag(s) `Frob

If you try to select a column from a table that's not in a from or join
clause, you get an error too:

```ocaml
let%sql query = select MyDB.my_table.name
```

    Table `my_table` not selected in query

There are many problems though. Off the top of my head:

* Everything must be lowercase;
* Error reporting sucks;
* You need parentheses around OCaml expressions that wouldn't be needed
  in real SQL;
* Lots of MySQL functions are not defined yet so you'll get bogus
  errors;
* Due to syntax clash you need stuff like `as_` or `in_`;
* Need to find a way to make it work when the DB definitions are in
  separate files;
* Conversion to string doesn't indent the query and is really
  parenthesis-happy.
* Everybody hates MySQL.
