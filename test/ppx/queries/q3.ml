let q3 =
  let%sql query, params = Mysql.(Expr.(Select.(
    let src = from Team.table in
    let src = inner_join (that Team.owner There) src in
    let src = inner_join (this Project.leader There) src in
    let sel =
      select Expr.(Vector.[
        field Team.name (Skip (Skip There)); field Project.title There
    ]) src in
    let stmt = where Expr.(field Project.title There =% "P%") sel in
    seal stmt
  ))) in
  query, params
