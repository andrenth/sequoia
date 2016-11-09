module Param = Sequoia_param

type build_step =
  { repr : string
  ; params : Param.t list
  ; pos : int
  }

let blank_step = { repr = ""; params = []; pos = 0 }

let build_param f st p =
  { repr = f st.pos
  ; params = [p]
  ; pos = st.pos + 1
  }

let join_lines =
  List.fold_left
    (fun acc s ->
      if s = "" then
        acc
      else if acc = "" then
        s
      else
        acc ^ "\n" ^ s)
    ""
