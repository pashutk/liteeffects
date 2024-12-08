type exp =
  | Int of int
  | Add of exp * exp
  | Lambda of string list * exp
  (* const name = exp; exp *)
  | Bound of string * exp * exp
  | Ref of string
  | Mult of exp * exp
