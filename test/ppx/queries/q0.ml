let q0 =
  let%sql query, params =
    Mysql.(Expr.(Select.(Expr.(Vector.(OrderBy.Expr.(Vector.(
      from Team.table
        |> left_join (that Team.owner)
        |> right_join (this Project.leader)
        |> select
             [ field User.id
             ; (foreign_key Team.owner) --> "the_owner"
             ; field User.name
             ; as_bool (field Team.id)
             ; field Team.name
             ; subquery (from User.table |> select [field User.name])
             ; field User.id + int 1
             ; date_add (date ~year:2016 ~month:10 ~day:20) 30 Days
             ; if_ (length (field User.name) > int 10)
                 (field User.name)
                 (string "short")
             ; if_ (is_null (field User.site))
                 (string "no site")
                 (unwrap User.site)
             ; field User.id =? [int 1; int 2; int 3]
             ]
        |> where
             (alias "the_owner" = int 1 && field User.name =% "foo%"
              && foreign_key Team.owner > int 0
              && is_not_null (field User.site)
              && field User.active = enum Bool.(instance True))
        |> group_by [field User.name]
        |> order_by [asc (alias "the_owner")]
        |> seal
    ))))))) in
  query, params
