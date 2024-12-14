open Ast

exception TypeError of string

let typeError msg = raise (TypeError msg)

let rec check ((term : exp), (expected : Ast.typ)) =
  match term with
  | Int _ -> if expected != TInt then typeError "Expected Int"
  | Lambda (params, Some return_type, _result) -> (
      match expected with
      | TLambda (expected_params, expected_result) ->
          if List.length params != List.length expected_params then
            typeError "Params list length mismatch";
          List.iter2
            (fun (name, typ) expected_typ ->
              if typ != expected_typ then
                typeError ("Param type mismatch: " ^ name))
            params expected_params;
          if return_type != expected_result then
            typeError "Return type mismatch"
      | _ -> typeError "Expected Lambda")
  | Lambda (params, None, result) ->
      check (Lambda (params, Some (synthesize result), result), expected)
  | Add (_left, _right) ->
      if expected != TInt then typeError "Add: Expected Int"
  | Bound (_name, _value, _next) -> ()
  | Ref _name -> ()
  | Mult (_left, _right) -> ()
  | App (_name, _args) -> ()
  | Perform (_effect, _action, _args) -> ()
  | Effect (_name, _actions) -> ()
  | Handle (_exp, _effect, _actions) -> ()

and synthesize (term : exp) =
  match term with Int _ -> TInt | Add (_, _) -> TInt | _ -> TInt
