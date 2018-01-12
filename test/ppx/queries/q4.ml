let q4 =
  let query, params = Mysql.(Expr.(Select.(
    let src = from User.table in
    let src = right_join (this Team.owner There) src in
    let src = right_join (this Project.leader (Skip There)) src in
    let sel = select Expr.(Vector.
      [ field Team.id There
      ; field Team.name There
      ; field User.id (Skip (Skip There))
      ; field User.name (Skip (Skip There))
      ])
      src in
    let stmt =
      where Expr.(field User.id (Skip (Skip There)) > int 42) sel in
    seal stmt
  ))) in
  query, params
