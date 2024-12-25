open Ast

type type_error =
  | Expected of Ast.typ
  | LambdaParamsCountMismatch
  | LambdaReturnTypeMismatch
  | LambdaParamTypeMismatch
  | Unknown

module StringMap = Map.Make (String)

type env_t = Ast.typ StringMap.t

let rec check (term : exp) (expected : Ast.typ) (env : env_t) :
    (unit, type_error) Result.t =
  match term with
  | Int _ -> if expected != TInt then Error (Expected TInt) else Ok ()
  | Lambda (params, Some return_type, _result) -> (
      match expected with
      | TLambda (expected_params, expected_result) ->
          if List.length params != List.length expected_params then
            Error LambdaParamsCountMismatch
          else if
            List.exists2
              (fun (_name, typ) expected_typ -> typ != expected_typ)
              params expected_params
          then Error LambdaParamTypeMismatch
          else if return_type != expected_result then
            Error LambdaReturnTypeMismatch
          else Ok ()
      | _ -> Error (Expected expected))
  | Lambda (params, None, result) ->
      check (Lambda (params, Some (synthesize result), result)) expected env
  | Add (left, right) ->
      if expected != TInt then Error (Expected TInt)
      else Result.bind (check left TInt env) (fun () -> check right TInt env)
  | Bound (_name, None, value, next) -> check value (synthesize next) env
  | Bound (_name, Some _typ, _value, _next) -> Error Unknown
  | Ref _name -> Error Unknown
  | Mult (_left, _right) -> Error Unknown
  | App (_name, _args) -> Error Unknown
  | Perform (_effect, _action, _args) -> Error Unknown
  | Effect (_name, _actions) -> Error Unknown
  | Handle (_exp, _effect, _actions) -> Error Unknown

and synthesize (term : exp) =
  match term with Int _ -> TInt | Add (_, _) -> TInt | _ -> TInt
