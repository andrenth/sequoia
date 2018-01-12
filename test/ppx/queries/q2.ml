let q2 =
  let query, params = Mysql.(Expr.(Select.(Expr.(OrderBy.Expr.(Vector.(
    let src = from TeamUser.table in
    let src = left_join (that TeamUser.user There) src in
    let src = left_join (that TeamUser.team (Skip There)) src in
    let sel =
      select ~distinct:true Expr.(Vector.[
        field User.name There; field Team.name (Skip There)
      ]) src in
    let stmt =
      where Expr.(field Team.name (Skip There) =% "foo%") sel in
    let stmt =
      order_by [asc (field Team.name (Skip There))] stmt in
    seal stmt
  )))))) in
  query, params
