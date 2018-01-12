let q7 =
  let query, params = Mysql.(Lit.(Vector.(Replace.(Vector.(
    replace
      ~into:User.table
      ~fields:[User.id; User.name; User.site]
      ~values:[
        [int 1; string "a"; Null.string "a.com"];
        [int 2; string "b"; Null.string "b.com"];
      ]
    |> seal
  ))))) in
  query, params
