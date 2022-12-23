open Sequoia_mysql

module Tables = struct
  module User = struct
    include (val Mysql.table "users")
    let id = Field.int "id"
    let name = Field.string "name"
  end

  module Team = struct
    include (val Mysql.table "teams")
    let id = Field.int "id"
    let owner = Field.foreign_key "owner_id" ~references:User.id
    let name = Field.string "name"
  end

  module TeamUser = struct
    include (val Mysql.table "teams_users")
    let id = Field.int "id"
    let team = Field.foreign_key "team_id" ~references:Team.id
    let user = Field.foreign_key "user_id" ~references:User.id
  end
  let _ = TeamUser.id   (* avoid unused warning *)
  let _ = TeamUser.team (* avoid unused warning *)
  let _ = TeamUser.user (* avoid unused warning *)

  module Project = struct
    include (val Mysql.table "projects")
    let id = Field.int "id"
    let leader = Field.foreign_key "leader_id" ~references:User.id
    let title = Field.string "title"
  end
  let _ = Project.id
  let _ = Project.title
end

let () =
  let open Tables in
  print_endline @@ fst @@ Mysql.(Expr.(Select.(Expr.(
    from Team.table
      |> left_join (that Team.owner There)
      |> right_join (this Project.leader There)
      |> select
          [ field User.id (Skip There)
          ; field User.name (Skip There)
          ; field Team.id (Skip (Skip There))
          ; field Team.name (Skip (Skip There))
          ]
      |> where
         (field User.id (Skip There) = foreign_key Project.leader There)
      |> seal
  ))))

(*
let () = print_endline "==="

let () =
  let open Tables in
  print_endline Sequoia.(
    from TeamUser.table
      |> that TeamUser.team  There
      |> that TeamUser.user  (Skip There)
      |> this   Project.leader (Skip There)
      |> select
          |> fields [field Team.name] There
          |> fields [field User.name] (Skip (Skip There))
      |> to_string
  )

let () =
  let open Tables in
  print_endline Sequoia.(
    let src = from TeamUser.table in
    let src = that TeamUser.user There src in
    let src = that TeamUser.team (Skip There) src in
    let sel = select src in
    let sel = fields [field User.name] There sel in
    let sel = fields [field Team.name] (Skip There) sel in
    to_string sel
  )

let () = print_endline "==="

let () =
  let open Tables in
  print_endline Sequoia.(
    let src = from Team.table in
    let src = that Team.owner There src in
    let src = this Project.leader There src in
    let sel = select src in
    let sel = fields [field Team.name] (Skip (Skip There)) sel in
    let sel = fields [field Project.title] There sel in
    to_string sel
  )

let () =
  let open Tables in
  print_endline Sequoia.(
    let src = from User.table in
    let src = this Team.owner There src in
    let src = this Project.leader (Skip There) src in
    let sel = select src in
    let sel = fields [field Team.id; field Team.name] There sel in
    let sel = fields [field User.id; field User.name] (Skip (Skip There)) sel in
    to_string sel
  )
*)
