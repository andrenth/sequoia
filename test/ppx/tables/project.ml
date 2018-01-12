module%sql Project = struct
  include (val Mysql.table "projects")
  let id = Field.int "id"
  let leader = Field.foreign_key "leader_id" ~references:User.id
  let title = Field.string "title"
end
