module type Elem = sig
  type ('a, 'b) elem
end

module type S = sig
  type ('a, 'b) elem

  type (_, _, _) t =
    | [] : ('a, 'b, Nat.z) t
    | (::) : ('a, 'b) elem * ('a, 'c, 'n) t -> ('a, 'b * 'c, 'n Nat.s) t

  type 'z folder = { f : 'a 'b. 'z -> ('a, 'b) elem -> 'z }

  val vector_fold_left : 'z folder -> 'z -> ('a, 'b, 'n) t -> 'z
  val vector_length : ('a, 'b, 'n) t -> int

  type ('a, 'b, 'm, 'n) matrix =
    | [] : ('a, 'b, Nat.z, 'n) matrix
    | (::) : ('a, 'b, 'n) t * ('a, 'b, 'm, 'n) matrix
          -> ('a, 'b, 'm Nat.s, 'n) matrix

  val matrix_fold_left : 'z folder -> 'z -> ('a, 'b, 'm, 'n) matrix -> 'z
end

module Make (E : Elem) : S
  with type ('a, 'b) elem := ('a, 'b) E.elem =
struct
  type (_, _, _) t =
    | [] : ('a, 'b, Nat.z) t
    | (::) : ('a, 'b) E.elem * ('a, 'c, 'n) t -> ('a, 'b * 'c, 'n Nat.s) t

  type 'z folder = { f : 'a 'b. 'z -> ('a, 'b) E.elem -> 'z }

  let rec vector_fold_left
    : type a b n. 'z folder -> 'z -> (a, b, n) t -> 'z =
    fun f z -> function
      | [] -> z
      | e::es -> vector_fold_left f (f.f z e) es

  let rec vector_length : type a b n. (a, b, n) t -> int = function
    | [] -> 0
    | _::xs -> 1 + vector_length xs

  type ('a, 'b, 'm, 'n) matrix =
    | [] : ('a, 'b, Nat.z, 'n) matrix
    | (::) : ('a, 'b, 'n) t * ('a, 'b, 'm, 'n) matrix
          -> ('a, 'b, 'm Nat.s, 'n) matrix

  (*type 'z matrix_folder = { mf : 'a 'b. 'z -> ('a, 'b) E.elem -> 'z }*)

  let rec matrix_fold_left
    : type a b m n. 'z folder -> 'z -> (a, b, m, n) matrix -> 'z =
    fun f z -> function
      | [] -> z
      | v::vs ->
          let z = vector_fold_left f z v in
          matrix_fold_left f z vs
end
