let pp_pair pp_first pp_second fmt (first, second) =
  Format.fprintf fmt "(%a, %a)" pp_first first pp_second second

let rec pp_ttype fmt = function
  | Ast.TInt -> Format.fprintf fmt "Int"
  | Ast.TLambda (params, result) ->
      Format.fprintf fmt "(%a) => %a"
        (Format.pp_print_list pp_ttype)
        params pp_ttype result

let pp_lambda_param fmt (name, ttype) =
  Format.fprintf fmt "%s: %a" name pp_ttype ttype

let pp_lambda_params fmt params =
  Format.fprintf fmt "(%a)" (Format.pp_print_list pp_lambda_param) params

let rec pp_ast fmt = function
  | Ast.Int i -> Format.fprintf fmt "Int %d" i
  | Ast.Ref name -> Format.fprintf fmt "Ref %s" name
  | Ast.Add (a, b) -> Format.fprintf fmt "Add (%a, %a)" pp_ast a pp_ast b
  | Ast.Mult (a, b) -> Format.fprintf fmt "Mult (%a, %a)" pp_ast a pp_ast b
  | Ast.Lambda (params, return_type, exp) -> (
      match return_type with
      | None ->
          Format.fprintf fmt "Lambda (%a => %a)" pp_lambda_params params pp_ast
            exp
      | Some ttype ->
          Format.fprintf fmt "Lambda (%a: %a => %a)" pp_lambda_params params
            pp_ttype ttype pp_ast exp)
  | Ast.App (id, args) ->
      Format.fprintf fmt "App (%s, %a)" id (Format.pp_print_list pp_ast) args
  | Ast.Perform (effect, action, args) ->
      Format.fprintf fmt "Perform (%s, %s, %a)" effect action
        (Format.pp_print_list pp_ast)
        args
  | Ast.Bound (name, None, value, exp) ->
      Format.fprintf fmt "Bound (%s, %a, %a)" name pp_ast value pp_ast exp
  | Ast.Bound (name, Some typ, value, exp) ->
      Format.fprintf fmt "Bound (%s of type %a: %a, %a)" name pp_ttype typ
        pp_ast value pp_ast exp
  | Ast.Effect (name, actions) ->
      Format.fprintf fmt "Effect (%s, %a)" name
        (Format.pp_print_list Format.pp_print_string)
        actions
  | Ast.Handle (exp, effect, actions) ->
      Format.fprintf fmt "Handle (%a, %s, %a)" pp_ast exp effect
        (Format.pp_print_list (pp_pair Format.pp_print_string pp_ast))
        actions
