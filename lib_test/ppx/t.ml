open Printf
open Tables
module Mysql = Sequoia_mysql

let print_params ps =
  printf "[\n%!";
  let print_param = function
    | Mysql.Param.Bool b -> printf "  Bool %b,\n%!" b
    | Mysql.Param.Int i -> printf "  Int %d,\n%!" i
    | Mysql.Param.String s -> printf "  String %s,\n%!" s
    | Mysql.Param.Enum (module E) -> printf "  %s,\n%!" (E.to_string)
    | _ -> printf "  <something else>,\n%!" in
  let rec print = function
    | [] -> printf "]\n%!"
    | p::ps -> print_param p; print ps in
  print ps

let () =
  let%sql query, params =
    Mysql.(Expr.(Select.(Expr.(Vector.(OrderBy.Expr.(Vector.(
      from Team.table
        |> left_join (that Team.owner)
        |> right_join (this Project.leader)
        |> select
             [ field User.id
             ; (foreign_key Team.owner) --> "the_owner"
             ; field User.name
             ; as_bool (field Team.id)
             ; field Team.name
             ; subquery (from User.table |> select [field User.name])
             ; field User.id + int 1
             ; date_add (date ~year:2016 ~month:10 ~day:20) 30 Days
             ; if_ (length (field User.name) > int 10)
                 (field User.name)
                 (string "short")
             ; if_ (is_null (field User.site))
                 (string "no site")
                 (unwrap User.site)
             ; field User.id =? [int 1; int 2; int 3]
             ]
        |> where
             (alias "the_owner" = int 1 && field User.name =% "foo%"
              && foreign_key Team.owner > int 0
              && is_not_null (field User.site)
              && field User.active = enum Bool.(instance True))
        |> group_by [field User.name]
        |> order_by [asc (alias "the_owner")]
        |> seal
    ))))))) in
  print_endline query;
  print_params params

let () = print_endline "==="

let () =
  let%sql query, params = Mysql.(Select.(Expr.(OrderBy.Expr.(Vector.(
    from TeamUser.table
      |> left_join (that TeamUser.team)
      |> right_join (that TeamUser.user)
      |> inner_join (this Project.leader)
      |> select ~distinct:true Expr.(Vector.
           [ field Team.name
           ; field User.name
           ])
      |> order_by [asc (field User.name); desc (field Team.name)]
      |> limit 10 ~offset:5
      |> seal
  ))))) in
  print_endline query;
  print_params params

let () = print_endline "==="

let () =
  let query, params = Mysql.(Expr.(Select.(Expr.(OrderBy.Expr.(Vector.(
    let src = from TeamUser.table in
    let src = left_join (that TeamUser.user There) src in
    let src = left_join (that TeamUser.team (Skip There)) src in
    let sel =
      select ~distinct:true Expr.(Vector.[
        field User.name There; field Team.name (Skip There)
      ]) src in
    let stmt =
      where Expr.(field Team.name (Skip There) =% "foo%") sel in
    let stmt =
      order_by [asc (field Team.name (Skip There))] stmt in
    seal stmt
  )))))) in
  print_endline query;
  print_params params

let () = print_endline "==="

let () =
  let%sql query, params = Mysql.(Expr.(Select.(
    let src = from Team.table in
    let src = inner_join (that Team.owner There) src in
    let src = inner_join (this Project.leader There) src in
    let sel =
      select Expr.(Vector.[
        field Team.name (Skip (Skip There)); field Project.title There
    ]) src in
    let stmt = where Expr.(field Project.title There =% "P%") sel in
    seal stmt
  ))) in
  print_endline query;
  print_params params

let () = print_endline "==="

let () =
  let query, params = Mysql.(Expr.(Select.(
    let src = from User.table in
    let src = right_join (this Team.owner There) src in
    let src = right_join (this Project.leader (Skip There)) src in
    let sel = select Expr.(Vector.
      [ field Team.id There
      ; field Team.name There
      ; field User.id (Skip (Skip There))
      ; field User.name (Skip (Skip There))
      ])
      src in
    let stmt =
      where Expr.(field User.id (Skip (Skip There)) > int 42) sel in
    seal stmt
  ))) in
  print_endline query;
  print_params params

let () = print_endline "==="

let () =
  let%sql query, params = Mysql.(Select.(Expr.(Vector.(
    from User.table
      |> left_join (self User.id User.id)
      |> select [ field User.id ]
      |> seal
  )))) in
  print_endline query;
  print_params params

let () = print_endline "==="

let () =
  let query, params = Mysql.(Lit.(Vector.(Insert.(Vector.(
    insert
      ~into:User.table
      ~fields:[User.id; User.name; User.site]
      ~values:[
        [int 1; string "a"; Null.string "a.com"];
        [int 2; string "b"; Null.string "b.com"];
      ]
    |> seal
  ))))) in
  print_endline query;
  print_params params

let () = print_endline "==="

let () =
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
  print_endline query;
  print_params params

let () = print_endline "==="

let () =
  let query, params =
    Mysql.(Expr.(Update.(Vector.(Expr.(Vector.(OrderBy.Expr.(Vector.(
      update User.table
        ~set:
          [ User.name, string "John Doe"
          ; User.site, Null.string "johndoe.com"
          ]
      |> where (field User.name = string "john doe")
      |> order_by [desc (field User.name)]
      |> limit 10
      |> seal
    )))))))) in
  print_endline query;
  print_params params

let () = print_endline "==="

let () =
  let query, params = Mysql.(Expr.(Delete.(Expr.(
    delete ~from:User.table ~where:(field User.name = string "john doe")
      |> seal
  )))) in
  print_endline query;
  print_params params
