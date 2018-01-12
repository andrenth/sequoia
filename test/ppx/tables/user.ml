module%sql User = struct
  include (val Mysql.table "users")
  let id = Field.int "id"
  let name = Field.string "name"
  let site = Field.Null.string "site"
  let created = Field.timestamp "created"
  let active = Field.enum (module Bool) "active"
end
