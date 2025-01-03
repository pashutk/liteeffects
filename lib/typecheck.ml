open Ast
open Typecheck_utils

let rec check (term : exp) (expected_type : Ast.typ)
    (expected_effects : string list) (env : TEnv.t) :
    (unit, type_error) Result.t =
  match term with
  | Int _ when expected_type == TInt -> Ok ()
  | Int _ -> Error (Expected (expected_type, TInt))
  | Lambda (params, _, Some return_type, _result) -> (
      match expected_type with
      | TLambda (expected_params, _, _)
        when List.length params != List.length expected_params ->
          Error LambdaParamsCountMismatch
      | TLambda (expected_params, _, _)
        when List.exists2
               (fun (_name, typ) expected_typ -> typ != expected_typ)
               params expected_params ->
          Error LambdaParamTypeMismatch
      | TLambda (_, _, expected_result) when return_type != expected_result ->
          Error LambdaReturnTypeMismatch
      | TLambda (_, _, _) -> Ok ()
      | _ ->
          Error
            (Expected
               (expected_type, TLambda (List.map snd params, None, return_type)))
      )
  | Lambda (params, _, None, result) ->
      check
        (Lambda (params, None, Some (synthesize result), result))
        expected_type expected_effects env
  | Add (left, right) when expected_type == TInt ->
      Result.bind (check left TInt expected_effects env) (fun () ->
          check right TInt expected_effects env)
  | Add (_, _) -> Error (Expected (expected_type, TInt))
  | Bound (name, None, value, next) ->
      check next expected_type expected_effects
        (env |> TEnv.add_binding name (synthesize value))
  | Bound (_name, Some _typ, _value, _next) -> Error Unknown
  | Ref name -> (
      match env |> TEnv.find_binding_opt name with
      | None -> Error (UndefinedVariable name)
      | Some typ when typ = expected_type -> Ok ()
      | Some typ -> Error (Expected (expected_type, typ)))
  | Mult (_left, _right) -> Error Unknown
  | App (name, args) -> (
      match env |> TEnv.find_binding_opt name with
      | None -> Error (UndefinedFunction name)
      | Some (Ast.TLambda (params, _, _))
        when List.length args != List.length params ->
          Error FunctionCallArgCountMismatch
      | Some (Ast.TLambda (params, None, ret_typ)) ->
          if
            List.for_all2
              (fun arg param ->
                Result.is_ok (check arg param expected_effects env))
              args params
          then
            if ret_typ = expected_type then Ok ()
            else Error FunctionApplicationReturnTypeMismatch
          else Error FunctionCallArgTypeMismatch
      | Some _ -> Error (IsNotAFunction name))
  | Perform (effect, _action, _args) when not (env |> TEnv.has_binding effect)
    ->
      Error (UnknownEffect effect)
  | Perform (_effect, _action, _args) -> Error Unknown
  | Effect (_name, _actions, _next) -> Error Unknown
  | Handle (_exp, effect_name, _actions) -> (
      match env |> TEnv.find_binding_opt effect_name with
      | Some (TEffect _) -> Ok ()
      | _ -> Error (UnknownEffect effect_name))

and synthesize (term : exp) =
  match term with
  | Int _ -> TInt
  | Add (_, _) -> TInt
  | Lambda (params, None, _, body) ->
      TLambda (params |> List.map snd, None, synthesize body)
  | Bound (_name, None, exp, _next) -> synthesize exp
  | _ ->
      failwith
        (Format.asprintf "Failed to synthesize type for the term %a"
           Ast_utils.pp_ast term)

let check_empty ast expected_type expected_effects =
  check ast expected_type expected_effects TEnv.empty

let check_main ast = check ast TInt [] TEnv.empty
