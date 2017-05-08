# Sequoia

Sequoia is a type-safe query builder for OCaml. It uses the OCaml type system
to ensure only fields from tables used as data sources for the query (i.e.
tables referred to in `FROM` or `JOIN` statements) can be used. It only allows
joins using fields previously declared to reference each other. Finally, it
prevents you from using expressions with incompatible types

The queries are composable and the library is extensible, currently providing
drivers for MySQL/MariaDB and SQLite.

## Example usage

### Table definition

Tables are defined via OCaml modules, each of its fields corresponding to
a value:

```ocaml
open Sequoia_mysql

module User = struct
  include (val Mysql.table "user")
  let id = Field.int "id"
  let name = Field.string "name"
end

module Publisher = struct
  include (val Mysql.table "publisher")
  let id = Field.int "id"
  let name = Field.string "name"
end

module Book = struct
  include (val Mysql.table "book")
  let id = Field.int "id"
  let owner = Field.foreign_key "owner_id" ~references:User.id
  let publisher = Field.foreign_key "publisher_id" ~references:Publisher.id
  let title = Field.string "title"
  let author = Field.string "author"
end

module BookUser = struct
  include (val Mysql.table "book_user")
  let book = Field.foreign_key "book_id" ~references:Book.id
  let user = Field.foreign_key "user_id" ~references:User.id
end
```

### `SELECT` queries

A `SELECT` query can be created like this:

```ocaml
let query, params =
	Mysql.(Expr.(Select.(Expr.(Vector.(OrderBy.Expr.(Vector.(
    from BookUser.table
      |> left_join (that BookUser.user There)
      |> left_join (that BookUser.book (Skip There))
      |> left_join (that Book.publisher (Skip There))
      |> select
           [ field User.name There
           ; field Book.title (Skip (Skip There))
           ; field Publisher.name (Skip There)
           ]
      |> where (field User.name There = field Book.author (Skip (Skip There)))
      |> order_by
           [ asc (field User.name There)
           ; desc (field Book.title (Skip (Skip There)))
           ]
      |> limit 10
      |> seal
)))))))
```

The epic sequence of local module opens makes the query cleaner. The following
modules are opened: `Mysql`, `Mysql.Expr` (MySQL expressions that work on
every query), `Mysql.Select` (functions for `SELECT` query creation),
`Mysql.Select.Expr` (MySQL expressions allowed only in `SELECT` queries),
`Mysql.Select.Expr.Vector` (vectors of such expressions), `OrderBy.Expr`
(`ORDER BY` expressions are attached to `ASC` or `DESC` specifications) and
`OrderBy.Expr.Vector` (vectors of `ORDER BY` expressions).

The `seal` function marks the end of the query and returns two values: a
string representation of the query, with markers for parameters according
to the prepared statement syntax of the driver, and the list of query
parameters itself.

The expression above will then generate the following query (though not with
such nice indentation, and a bit more parenthesis-happy):

```sql
SELECT
  user.name, book.title publisher.name
FROM
  book_user
LEFT JOIN
  user ON user.id = book_user.user_id
LEFT JOIN
  book ON book.id = book_user.book_id
LEFT JOIN
  publisher ON publisher.id = book.publisher_id
WHERE
  user.name = book.author
ORDER BY
  user.name ASC, book.title DESC
LIMIT ?
```

and the parameter list will be `[Param.Int 10]`.

#### The Skip/There stuff

You have probably noticed the odd `There`, `Skip There` and `Skip (Skip There)`
values in the query expression above. These are used to ensure that only fields
from previously referenced tables can be used, and are henceforth called
"referrer arguments".

You can think of then as walking on a linked list of tables until you find the
table you're referring to. So, for example, the expression `Skip (Skip There)`
would be used to refer to the third table in that list.

To know how to refer to a table in this scheme, it is necessary to understand
how the table list is built. The process works as follows for the above example.

1. A call to the `from` function creates a singleton list. In the example
above, this would be a list containing the `book_user` table: `[book_user]`.

2. A join statement in SQL (in the example above, the `left_join` function)
adds a new data source to the query. Following the example, a join is performed
with the `user` table via the `book_user.user_id` reference. To be able to
refer to the `book_user` table, we must walk the list above until we find it,
and since it's the only element of the list, it's already `There`. Table `user`
is added to the list, which now looks like `[user; book_user]`.

3. A second join is made, this time with the `book` table, in an way analogous
to the description above. Only now the `book_user` table is no longer the
first element of the list, and to find it we must skip the `user` table,
resulting in `Skip There`. A new insertion is always made immediately before
the referred table in the list, so it now looks like this:
`[user; book; book_user]`.

4. Yet another join, this time with the `publisher` table via the foreign
key `book.publisher_id`. Since `book` is now second table in the list, we
once again have `Skip There`. The final list looks like
`[user; publisher; book; books_user]`.

The `select` function marks the end of the list construction, so from this
point onwards fields can be referenced according to it:

* To refer to a field in `user`, use `There`;
* To refer to a field in `publisher`, use `Skip There`;
* To refer to a field in `book`, use `Skip (Skip There)`;
* To refer to a field in `book_user`, use `Skip (Skip (Skip There))`.

Note that the use of a linked list to explain the construction of the referrer
arguments is just a didactic device. The actual implementation uses the OCaml
type system to ensure query correctness, and there are no type-level lists in
OCaml, so function types are used instead.

#### The syntax extension

According to extensive research with two developers, usage of the `Skip`/`There`
stuff in the real world "would not that bad". However, it would be nice to have
them generated automatically, if only to make the queries look cleaner.

Therefore, Sequoia includes a PPX syntax extension to do exactly that. If
you decide to use it, simply append `%sql` to the `module` and `let` keywords
used to define tables and query expressions, respectively.

The example above, with the sintax extension, would look like this:

```ocaml
open Sequoia_mysql

module%sql User = struct
  include (val Mysql.table "user")
  let id = Field.int "id"
  let name = Field.string "name"
end

module%sql Publisher = struct
  include (val Mysql.table "publisher")
  let id = Field.int "id"
  let name = Field.string "name"
end

module%sql Book = struct
  include (val Mysql.table "book")
  let id = Field.int "id"
  let owner = Field.foreign_key "owner_id" ~references:User.id
  let publisher = Field.foreign_key "publisher_id" ~references:Publisher.id
  let title = Field.string "title"
  let author = Field.string "author"
end

module%sql BookUser = struct
  include (val Mysql.table "book_user")
  let book = Field.foreign_key "book_id" ~references:Book.id
  let user = Field.foreign_key "user_id" ~references:User.id
end

let%sql query, params =
  Mysql.(Expr.(Select.(Expr.(Vector.(OrderBy.Expr.(Vector.(
    from BookUser.table
      |> left_join (that BookUser.user)
      |> left_join (that BookUser.book)
      |> left_join (that Book.publisher)
      |> select
           [ field User.name
           ; field Book.title
           ; field Publisher.name
           ]
      |> where (field User.name = field Book.author)
      |> order_by
           [ asc (field User.name)
           ; desc (field Book.title)
           ]
      |> limit 10
      |> seal
  )))))))
```

Please note that the syntax extension only works when writing the query in
the style above, using the `|>` operator, that is, with partial evaluation.
This is because it detects the locations where the referrer arguments must
be inserted by looking for single-argument `field` calls.

In the future it might be extended to be more comprehensive.

#### On `JOIN`s

You have probably noticed the use of `that` in the join expressions above.
It is used to create a join with the table that is referenced by the given
foreign key. For example, `left_join (that BookUser.user)` creates a join with
the `User` table, because the `BookUser.user` foreign key references
a field from `User`, namely `User.id`. Similarly, Sequoia also offers `this`
for joins with the table that contains the foreign key.

This behavior of requiring primary and foreign key pairs is by design, because
it allows for some extra validation on whether fields used in the join condition
actually do refer to the same thing.

This is based on my SQL usage, where the need for arbitrary expressions in join
conditionas has not appeared. I could be persuaded that it is important though.

### `INSERT` and `REPLACE` queries

Below is an example of an `INSERT` query. Currently only literal values are
supported for insertion.

```ocaml
let query, params = Mysql.(Lit.(Vector.(Insert.(Vector.(
  insert
    ~into:User.table
    ~fields:[User.id; User.name]
    ~values:[
      [int 1; string "Joe"];
      [int 2; string "Mary"];
    ]
  |> seal
)))))
```

The query ensures that the `fields` vector has the same length as each of the
vectors in `values`, and that their respective elements match. There is no
check for missing non-NULL fields, though this could be added in the future.

`REPLACE` queries follow the same structure, but for those, open the `Replace`
module instead of `Insert` and call the `replace` function instead of `insert`.

### `UPDATE` queries

```ocaml
let query, params = Mysql.(Expr.(Update.(Vector.(Expr.(Vector.(
  update Book.table
    ~set:
      [ Book.title,  string "King Sequoia"
      ; Book.author, string "William C. Tweed"
      ]
  |> where (field Book.title =% string "seq%") (* (=%) is the LIKE operator *)
  |> seal
))))))
```

### `DELETE` queries

```ocaml
let query, params = Mysql.(Expr.(Delete.(Expr.(
  delete
    ~from:Book.table
    ~where:(field Book.title <>% string "%sequoia%") (* (<>%) is the NOT LIKE operator *)
  |> seal
))))
```

## Issues and limitations

* No arbitrary expressions on joins;
* All table definitions must be in the same file;
* Queries can be defined in separate files but this is implemented in a
  hackish way (`Marshal` dump files in `/tmp`);
* `INSERT` queries don't forbid absent non-NULL fields;
* No ocamldoc for drivers (yet).

## Acknowledgements

Thanks to Gabriel Scherer for the [type-system wizardry](https://sympa.inria.fr/sympa/arc/caml-list/2016-09/msg00111.html) that makes this library possible.
