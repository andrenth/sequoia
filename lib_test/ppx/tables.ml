module Mysql = Sequoia_mysql

module%sql User = struct
  include (val Mysql.table "users")
  let id = Field.int "id"
  let name = Field.string "name"
  let site = Field.Null.string "site"
  let created = Field.timestamp "created"
end

module%sql Team = struct
  include (val Mysql.table "teams")
  let id = Field.int "id"
  let owner = Field.foreign_key "owner_id" ~references:User.id
  let name = Field.string "name"
end

module%sql TeamUser = struct
  include (val Mysql.table "teams_users")
  let id = Field.int "id"
  let team = Field.foreign_key "team_id" ~references:Team.id
  let user = Field.foreign_key "user_id" ~references:User.id
end

module%sql Project = struct
  include (val Mysql.table "projects")
  let id = Field.int "id"
  let leader = Field.foreign_key "leader_id" ~references:User.id
  let title = Field.string "title"
end
