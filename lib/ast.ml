type exp =
  | Int of int
  | Add of exp * exp
  | Const of string * exp
  | Function of string * string list * exp
