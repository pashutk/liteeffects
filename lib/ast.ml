type exp =
  | Int of int
  | Add of exp * exp
  | Lambda of string list * exp
  (* const name = exp; exp *)
  | Bound of string * exp * exp
  | Ref of string
  | Mult of exp * exp
  | App of string * exp list
  | Perform of string * string * exp list
  | Effect of string * string list
  | Handle of exp * string * (string * exp) list
