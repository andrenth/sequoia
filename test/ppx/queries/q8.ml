let q8 =
  let query, params =
    Mysql.(Expr.(Update.(Vector.(Expr.(OrderBy.Expr.(Vector.(
      update User.table
        ~set:
          [ User.name, string "John Doe"
          ; User.site, Null.string "johndoe.com"
          ]
      |> where (field User.name = string "john doe")
      |> order_by [desc (field User.name)]
      |> limit 10
      |> seal
    ))))))) in
  query, params
