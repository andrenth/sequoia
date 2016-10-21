module Tables = struct
  module User = struct
    include (val Sequoia.table "users")
    let id = Field.int "id"
    let name = Field.string "name"
  end

  module Team = struct
    include (val Sequoia.table "teams")
    let id = Field.int "id"
    let owner = Field.foreign_key "owner_id" ~references:User.id
    let name = Field.string "name"
  end

  module TeamUser = struct
    include (val Sequoia.table "teams_users")
    let id = Field.int "id"
    let team = Field.foreign_key "team_id" ~references:Team.id
    let user = Field.foreign_key "user_id" ~references:User.id
  end

  module Project = struct
    include (val Sequoia.table "projects")
    let id = Field.int "id"
    let leader = Field.foreign_key "leader_id" ~references:User.id
    let title = Field.string "title"
  end
end

let () =
  let open Tables in
  print_endline Sequoia.(
    from Team.table
      |> belonging_to Team.owner There
      |> having_one Project.leader There
      |> select
          |> fields [field User.id; field User.name] (Skip There)
          |> fields [field Team.id; field Team.name] (Skip (Skip There))
          |> fields [all Project.table]              There
      |> where Expr.(
           field User.id (Skip There) = foreign_key Project.leader There
         )
      |> to_string
  )

(*
let () = print_endline "==="

let () =
  let open Tables in
  print_endline Sequoia.(
    from TeamUser.table
      |> belonging_to TeamUser.team  There
      |> belonging_to TeamUser.user  (Skip There)
      |> having_one   Project.leader (Skip There)
      |> select
          |> fields [field Team.name] There
          |> fields [field User.name] (Skip (Skip There))
      |> to_string
  )

let () =
  let open Tables in
  print_endline Sequoia.(
    let src = from TeamUser.table in
    let src = belonging_to TeamUser.user There src in
    let src = belonging_to TeamUser.team (Skip There) src in
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
    let src = belonging_to Team.owner There src in
    let src = having_one Project.leader There src in
    let sel = select src in
    let sel = fields [field Team.name] (Skip (Skip There)) sel in
    let sel = fields [field Project.title] There sel in
    to_string sel
  )

let () =
  let open Tables in
  print_endline Sequoia.(
    let src = from User.table in
    let src = having_one Team.owner There src in
    let src = having_one Project.leader (Skip There) src in
    let sel = select src in
    let sel = fields [field Team.id; field Team.name] There sel in
    let sel = fields [field User.id; field User.name] (Skip (Skip There)) sel in
    to_string sel
  )
*)
