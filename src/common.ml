module AliasSet = Set.Make(struct
  type t = string
  let compare = compare
end)

type build_step =
  { repr : string
  ; params : Param.t list
  ; pos : int
  ; aliases : AliasSet.t
  }

let blank_step =
  { repr = ""
  ; params = []
  ; pos = 0
  ; aliases = AliasSet.empty
  }

let build_param f st p =
  { st with
    repr = f st.pos
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
