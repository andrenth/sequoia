let q9 =
  let query, params = Mysql.(Expr.(Delete.(Expr.(
    delete ~from:User.table ~where:(field User.name = string "john doe") ()
      |> seal
  )))) in
  query, params
