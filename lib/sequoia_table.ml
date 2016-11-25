type 'a t = { name : string }

let called name = { name }
let name { name } = name
