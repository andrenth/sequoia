let q1 =
  let%sql query, params = Mysql.(Select.(Expr.(OrderBy.Expr.(Vector.(
    from TeamUser.table
      |> left_join (that TeamUser.team)
      |> right_join (that TeamUser.user)
      |> inner_join (this Project.leader)
      |> select ~distinct:true Expr.(Vector.
           [ field Team.name
           ; field User.name
           ])
      |> order_by [asc (field User.name); desc (field Team.name)]
      |> limit 10 ~offset:5
      |> seal
  ))))) in
  query, params
