open Printf
module Mysql = Sequoia_mysql

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

let print_params ps =
  printf "[\n%!";
  let print_param = function
    | Mysql.Param.Int i -> printf "  Int %d,\n%!" i
    | Mysql.Param.String s -> printf "  String %s,\n%!" s
    | Mysql.Param.Bool b -> printf "  Bool %b,\n%!" b
    | _ -> printf "  <something else>,\n%!" in
  let rec print = function
    | [] -> printf "]\n%!"
    | p::ps -> print_param p; print ps in
  print ps

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

let () =
  print_endline query;
  print_params params
