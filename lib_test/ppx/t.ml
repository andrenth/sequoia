open Printf
open Tables
module Mysql = Sequoia_mysql

let print_params ps =
  printf "[\n%!";
  let print_param = function
    | Mysql.Param.Int i -> printf "  Int %d,\n%!" i
    | Mysql.Param.String s -> printf "  String %s,\n%!" s
    | Mysql.Param.Bool b -> printf "  Bool %b,\n%!" b
    | _ -> printf "  <something else>,\n%!" in
  let rec print = function
    | [] -> printf "]\n%!"
    | p::ps -> print_param p; print ps in
  print ps

let () =
  let%sql query, params = Mysql.(Select.(Expr.(Vector.(
    from Team.table
      |> left_join (belonging_to Team.owner)
      |> right_join (having_one Project.leader)
      |> select
           [ field User.id
           ; field User.name
           ; field Team.id
           ; field Team.name
           ; subquery (from User.table |> select [field User.name])
           ; field User.id + int 1
           ; date_add (date ~year:2016 ~month:10 ~day:20) 30 Lit.Days
           ; if_ (length (field User.name) > int 10)
               (field User.name)
               (string "short")
           ; if_ (is_null (field User.site))
               (string "no site")
               (unwrap User.site)
           ; field User.id =? [int 1; int 2; int 3]
           ]
      |> where (field User.name =% "foo%" && is_not_null (field User.site))
      |> order_by [field User.name]
      |> limit 10
      |> seal
  )))) in
  print_endline query;
  print_params params

let () = print_endline "==="

let () =
  let%sql query, params = Mysql.(Select.(
    from TeamUser.table
      |> left_join (belonging_to TeamUser.team)
      |> right_join (belonging_to TeamUser.user)
      |> inner_join (having_one Project.leader)
      |> select Expr.(Vector.
           [ field Team.name
           ; field User.name
           ])
      |> order_by Expr.(Vector.[field User.name; field Team.name])
      |> limit 10 ~offset:5
      |> seal
  )) in
  print_endline query;
  print_params params

let () = print_endline "==="

let () =
  let query, params = Mysql.(Select.(
    let src = from TeamUser.table in
    let src = left_join (belonging_to TeamUser.user There) src in
    let src = left_join (belonging_to TeamUser.team (Skip There)) src in
    let sel =
      select Expr.(Vector.[
        field User.name There; field Team.name (Skip There)
      ]) src in
    let stmt =
      where Expr.(field Team.name (Skip There) =% "foo%") sel in
    let stmt = order_by Expr.(Vector.[field Team.name (Skip There)]) stmt in
    seal stmt
  )) in
  print_endline query;
  print_params params

let () = print_endline "==="

let () =
  let%sql query, params = Mysql.(Select.(
    let src = from Team.table in
    let src = inner_join (belonging_to Team.owner There) src in
    let src = inner_join (having_one Project.leader There) src in
    let sel =
      select Expr.(Vector.[
        field Team.name (Skip (Skip There)); field Project.title There
    ]) src in
    let stmt = where Expr.(field Project.title There =% "P%") sel in
    seal stmt
  )) in
  print_endline query;
  print_params params

let () = print_endline "==="

let () =
  let query, params = Mysql.(Select.(
    let src = from User.table in
    let src = right_join (having_one Team.owner There) src in
    let src = right_join (having_one Project.leader (Skip There)) src in
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
  )) in
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
    |> build
  ))))) in
  print_endline query;
  print_params params
