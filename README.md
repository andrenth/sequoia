# Sequoia

Sequoia is a type-safe query builder for OCaml. It uses the OCaml type system
to ensure only fields from tables used as data sources for the query (i.e.
tables referred to in `FROM` or `JOIN` statements) can be used. It only allows
joins using fields previously declared to reference each other. Finally, it
prevents you from using expressions with incompatible types

The queries are composable and the library is extensible, currently providing
drivers for MySQL/MariaDB and SQlite.

## Example usage

Tables are defined via OCaml modules, each of its fields corresponding to
a value:

```ocaml
module Mysql = Sequoia_mysql

module User = struct
  include (val Mysql.table "users")
  let id = Field.int "id"
  let name = Field.string "name"
end

module Book = struct
  include (val Mysql.table "books")
  let id = Field.int "id"
  let owner = Field.foreign_key "owner_id" ~references:User.id
  let publisher = Field.foreign_key "publisher_id" ~references:Publisher.id
  let title = Field.string "title"
  let author = Field.string "author"
end

module Publisher = struct
  include (val Mysql.table "books_user")
  let id = Field.int "id"
  let name = Field.string "name"
end

module BookUser = struct
  include (val Mysql.table "publishers")
  let book = Field.foreign_key "book_id" ~references:Book.id
  let user = Field.foreign_key "user_id" ~references:User.id
end
```

A `SELECT` query can be created like this:

```ocaml
let query, params = Mysql.(Select.(Expr.(
  from BookUser.table
    |> left_join belonging_to BookUser.user There
    |> left_join belonging_to BookUser.book (Skip There)
    |> left_join belonging_to Book.publisher (Skip There)
    |> select
         [ field User.name (Skip (Skip There))
         ; field Book.title (Skip There)
         ; field Publisher.name There
         ]
    |> where
         (field User.name (Skip (Skip There)) = field Book.author (Skip There))
    |> order_by
         [ field User.name (Skip (Skip There))
         ; field Book.title There
         ]
    |> limit 10
    |> seal
)))
```

The `seal` function marks the end of the query and returns two values: a
string representation of the query, with markers for parameters according
to the prepared statement syntax of the driver, and the list of query
parameters itself.

The expression above will then generate the following query:

```sql
SELECT
  users.name, boooks.title publishers.name
FROM
  books_users
LEFT JOIN
  users on users.id = books_users.user_id
LEFT JOIN
  books on books.id = books_users.book_id
LEFT JOIN
  publishers on publishers.id = books.publisher_id
WHERE
  users.name = books.author
ORDER BY
  (users.name, books.title)
LIMIT ?
```
and the parameter list will be `[Param.Int 10]`.

## The Skip/There stuff

You have probably noticed the odd `There`, `Skip There` and `Skip (Skip There)`
values in the query expression above. These are used to ensure that only fields
from previously referenced tables can be used, and are called "referrer
arguments".

You can think of then as walking on a linked list of tables until you find the
table you're referring to. So, for example, the expression `Skip (Skip There)`
would be used to refer to the third table in that list.

To know how to find the table in this scheme, it is necessary to understand
how the list is built. The process works as follows.

1. A call to the `from` function creates a singleton list. In the example
above, this would be a list containing the `books_users` table: `[books_users]`.

2. A join statement in SQL (in the example above, the `left_join` function)
adds a new data source to the query. Following the example, there a join is
made with the `users` table via the `books_users.user_id` reference. To be
able to refer to the `books_users` table, we must walk the list above until
we find it, and since it's the only element of the list, it's already `There`.
The list now looks like `[users; books_users]`.

3. A second join is made, this time with the `books` table, in an way analogous
to the description above. Only now the `books_users` table is no longer the
first element of the list, so to find it we must skip the `users` table and
therefore we have `Skip There`. A new insertion is always made immediately
before the referred table in the list, which then looks like this:
`[users; books; books_users]`.

4. Yet another join, this time with the `publishers` table via the foreign
key `books.publisher_id`. Since `books` is the second table in the list, we
once again have `Skip There`. The final list looks like
`[users; publishers; books; books_users]`.

The `select` function marks the end of the list construction, so from this
point onwards fields can be referenced according to it:

* To refer to a field in the `users` table, use `There`
* To refer to a field in the `publishers` table, use `Skip There`
* To refer to a field in the `books` table, use `Skip (Skip There)`
* To refer to a field in the `books_users` table, use `Skip (Skip (Skip There))`

Note that the use of a linked list to explain the construction of the referrer
arguments is just a didactic device. The actual implementation uses the OCaml
type system to ensure query correctness, and there are no type-level lists in
OCaml, so function types are used instead.

## The syntax extension

According to extensive research with two developers, the `Skip`/`There` stuff
is "not that bad". However, it would be nice to have them generated
automatically, if only to make the queries look cleaner.

Therefore, Sequoia includes a PPX syntax extension to do exactly that. If
you decide to use it, simply append `%sql` to the `module` and `let` keywords
used to define a table and query expression, respectively.

The example above, with the sintax extension, would look like this:

```ocaml
module Mysql = Sequoia_mysql

module%sql User = struct
  include (val Mysql.table "users")
  let id = Field.int "id"
  let name = Field.string "name"
end

module%sql Publisher = struct
  include (val Mysql.table "publishers")
  let id = Field.int "id"
  let name = Field.string "name"
end

module%sql Book = struct
  include (val Mysql.table "books")
  let id = Field.int "id"
  let owner = Field.foreign_key "owner_id" ~references:User.id
  let publisher = Field.foreign_key "publisher_id" ~references:Publisher.id
  let title = Field.string "title"
  let author = Field.string "author"
end

module%sql BookUser = struct
  include (val Mysql.table "books_user")
  let book = Field.foreign_key "book_id" ~references:Book.id
  let user = Field.foreign_key "user_id" ~references:User.id
end

let () =
  let%sql query, params = Mysql.(Select.(Expr.(
    from BookUser.table
      |> left_join belonging_to BookUser.user
      |> left_join belonging_to BookUser.book
      |> left_join belonging_to Book.publisher
      |> select
           [ field User.name
           ; field Book.title
           ; field Publisher.name
           ]
      |> where (field User.name) = field Book.author)
      |> order_by [field User.name; field Book.title]
      |> limit 10
      |> seal
  ))) in
  print_endline query

```

Please note that the syntax extension only works when writing the query in
the style above, using the `|>` operator, that is, with partial evaluation.
This is because it detects the locations where the referrer arguments must
be inserted by looking for single-argument `field` calls.

In the future it might be extended to be more comprehensive.

## Issues and limitations

* No arbitrary expressions on joins;
* No `INSERT`, `UPDATE` or `DELETE` support (yet).
* No documentation other than this file (yet).
