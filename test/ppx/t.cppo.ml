open Sequoia_mysql

module Bool = struct
  type t =
    | True
    | False

  let instance = function
    | True ->
        (module struct
          type t = True
          let to_string = "TRUE"
          let _ = True (* avoid warning *)
        end : Mysql.Enum.Instance)
    | False ->
        (module struct
          type t = False
          let to_string = "FALSE"
	  let _ = False
          end : Mysql.Enum.Instance)

end

module Tables = struct
  #include "tables/user.ml"
  #include "tables/team.ml"
  #include "tables/project.ml"
  #include "tables/team_user.ml"
end

open Printf
open Sequoia_mysql

open Tables
#include "queries/q0.ml"
#include "queries/q1.ml"
#include "queries/q2.ml"
#include "queries/q3.ml"
#include "queries/q4.ml"
#include "queries/q5.ml"
#include "queries/q6.ml"
#include "queries/q7.ml"
#include "queries/q8.ml"
#include "queries/q9.ml"

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

let run =
  List.iter
    (fun (query, params) ->
      print_endline query;
      print_params params;
      print_endline "===")

let () =
  run [ q0
      ; q1
      ; q2
      ; q3
      ; q4
      ; q5
      ; q6
      ; q7
      ; q8
      ; q9
      ]
