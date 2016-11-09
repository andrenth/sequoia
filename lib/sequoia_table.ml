type 'a t = { name : string }

let called name = { name }
let to_string { name } = name
