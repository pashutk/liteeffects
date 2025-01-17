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
        (Lambda (params, None, Some (fst (synthesize result)), result))
        expected_type expected_effects env
  | Add (left, right) when expected_type == TInt ->
      Result.bind (check left TInt expected_effects env) (fun () ->
          check right TInt expected_effects env)
  | Add (_, _) -> Error (Expected (expected_type, TInt))
  | Bound (name, None, value, next) ->
      check next expected_type expected_effects
        (env |> TEnv.add_binding name (fst (synthesize value)))
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
  | Perform (effect, _action, _args) -> (
      match env |> TEnv.find_effect_opt effect with
      | None -> Error (UnknownEffect effect)
      | Some _ -> Error Unknown)
  | Effect (name, actions, next) ->
      let actions_map = StringMap.of_seq (List.to_seq actions) in
      check next expected_type expected_effects
        (env |> TEnv.add_effect name actions_map)
  | Handle (_exp, effect_name, _actions) -> (
      match env |> TEnv.find_effect_opt effect_name with
      | Some _ -> Ok ()
      | _ -> Error (UnknownEffect effect_name))

and synthesize (term : exp) : Ast.typ * EffectSet.t =
  match term with
  | Int _ -> (TInt, EffectSet.empty)
  | Add (left, right) ->
      (TInt, EffectSet.union (synthesize_effect left) (synthesize_effect right))
  | Lambda (params, Some effects, _, body) ->
      (TLambda (params |> List.map snd, None, fst (synthesize body)), effects)
  | Lambda (params, None, _, body) ->
      ( TLambda (params |> List.map snd, None, fst (synthesize body)),
        EffectSet.empty )
  | Bound (_name, None, exp, _next) -> synthesize exp
  | _ ->
      failwith
        (Format.asprintf "Failed to synthesize type for the term %a"
           Ast_utils.pp_ast term)

and synthesize_effect term =
  match term with
  | Int _ -> EffectSet.empty
  | Add (left, right) ->
      EffectSet.union (synthesize_effect left) (synthesize_effect right)
  | Lambda (_params, Some effects, _return, _next) -> effects
  | Perform (effect, _action, args) ->
      let current_effect = EffectSet.singleton effect in
      let arg_effects =
        args |> List.map synthesize_effect
        (* merge EffectSet list into single EffectSet *)
        |> List.fold_left EffectSet.union EffectSet.empty
      in
      EffectSet.union current_effect arg_effects
  | _ ->
      failwith
        (Format.asprintf "Failed to synthesize effect type for the term %a"
           Ast_utils.pp_ast term)

let check_empty ast expected_type expected_effects =
  check ast expected_type expected_effects TEnv.empty

let check_main ast = check ast TInt [] TEnv.empty
