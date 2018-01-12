module%sql Team = struct
  include (val Mysql.table "teams")
  let id = Field.int "id"
  let owner = Field.foreign_key "owner_id" ~references:User.id
  let name = Field.string "name"
end
