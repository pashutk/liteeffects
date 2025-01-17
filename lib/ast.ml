module EffectSet = Set.Make (String)

type lambda_effects = EffectSet.t option
type typ = TInt | TLambda of typ list * lambda_effects * typ
type effect = (string * typ) list

type exp =
  | Int of int
  | Add of exp * exp
  | Lambda of (string * typ) list * lambda_effects * typ option * exp
  (* const name = exp; exp *)
  | Bound of string * typ option * exp * exp
  | Ref of string
  | Mult of exp * exp
  | App of string * exp list
  | Perform of string * string * exp list
  | Effect of string * (string * typ) list * exp
  | Handle of exp * string * (string * exp) list
