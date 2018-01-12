module%sql TeamUser = struct
  include (val Mysql.table "teams_users")
  let id = Field.int "id"
  let team = Field.foreign_key "team_id" ~references:Team.id
  let user = Field.foreign_key "user_id" ~references:User.id
end
