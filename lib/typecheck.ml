open Ast
open Typecheck_utils

type type_error =
  (* Expected A but got B *)
  | Expected of Ast.typ * Ast.typ
  | LambdaParamsCountMismatch
  | LambdaReturnTypeMismatch
  | LambdaParamTypeMismatch
  | UndefinedFunction of string
  | IsNotAFunction of string
  | FunctionCallArgCountMismatch
  | FunctionCallArgTypeMismatch
  | FunctionApplicationReturnTypeMismatch
  | UndefinedVariable of string
  | Unknown

let rec check (term : exp) (expected : Ast.typ) (env : env_t) :
    (unit, type_error) Result.t =
  match term with
  | Int _ ->
      if expected != TInt then Error (Expected (expected, TInt)) else Ok ()
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
      | _ ->
          Error
            (Expected (expected, TLambda (List.map snd params, return_type))))
  | Lambda (params, None, result) ->
      check (Lambda (params, Some (synthesize result), result)) expected env
  | Add (left, right) ->
      if expected != TInt then Error (Expected (expected, TInt))
      else Result.bind (check left TInt env) (fun () -> check right TInt env)
  | Bound (name, None, value, next) ->
      check next expected (StringMap.add name (synthesize value) env)
  | Bound (_name, Some _typ, _value, _next) -> Error Unknown
  | Ref name -> (
      match StringMap.find_opt name env with
      | None -> Error (UndefinedVariable name)
      | Some typ when typ = expected -> Ok ()
      | Some typ -> Error (Expected (expected, typ)))
  | Mult (_left, _right) -> Error Unknown
  | App (name, args) -> (
      match StringMap.find_opt name env with
      | None -> Error (UndefinedFunction name)
      | Some (Ast.TLambda (params, ret_typ)) ->
          if List.length args != List.length params then
            Error FunctionCallArgCountMismatch
          else if
            List.for_all2
              (fun arg param -> Result.is_ok (check arg param env))
              args params
          then
            if ret_typ = expected then Ok ()
            else Error FunctionApplicationReturnTypeMismatch
          else Error FunctionCallArgTypeMismatch
      | Some _ -> Error (IsNotAFunction name))
  | Perform (_effect, _action, _args) -> Error Unknown
  | Effect (_name, _actions) -> Error Unknown
  | Handle (_exp, _effect, _actions) -> Error Unknown

and synthesize (term : exp) =
  match term with Int _ -> TInt | Add (_, _) -> TInt | _ -> TInt
