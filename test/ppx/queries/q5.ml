let q5 =
  let%sql query, params = Mysql.(Select.(Expr.(Vector.(
    from User.table
      |> left_join (self User.id User.id)
      |> select [ field User.id ]
      |> seal
  )))) in
  query, params
