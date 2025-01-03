let pp_pair pp_first pp_second fmt (first, second) =
  Format.fprintf fmt "(%a, %a)" pp_first first pp_second second

let pp_effects fmt effects =
  Format.fprintf fmt "<%a>"
    (Format.pp_print_list Format.pp_print_string)
    effects

let rec pp_ttype fmt = function
  | Ast.TInt -> Format.fprintf fmt "Int"
  | Ast.TLambda (params, None, result) ->
      Format.fprintf fmt "(%a) => %a"
        (Format.pp_print_list pp_ttype)
        params pp_ttype result
  | Ast.TLambda (params, Some effects, result) ->
      Format.fprintf fmt "(%a) => <%a> %a"
        (Format.pp_print_list pp_ttype)
        params pp_effects effects pp_ttype result
  | Ast.TEffect actions ->
      Format.fprintf fmt "Effect [%a]"
        (Format.pp_print_list (pp_pair Format.pp_print_string pp_ttype))
        actions

let pp_lambda_param fmt (name, ttype) =
  Format.fprintf fmt "%s: %a" name pp_ttype ttype

let pp_lambda_params fmt params =
  Format.fprintf fmt "(%a)" (Format.pp_print_list pp_lambda_param) params

let rec pp_ast fmt = function
  | Ast.Int i -> Format.fprintf fmt "Int %d" i
  | Ast.Ref name -> Format.fprintf fmt "Ref %s" name
  | Ast.Add (a, b) -> Format.fprintf fmt "Add (%a, %a)" pp_ast a pp_ast b
  | Ast.Mult (a, b) -> Format.fprintf fmt "Mult (%a, %a)" pp_ast a pp_ast b
  | Ast.Lambda (params, None, None, exp) ->
      Format.fprintf fmt "Lambda (%a => %a)" pp_lambda_params params pp_ast exp
  | Ast.Lambda (params, Some effects, None, exp) ->
      Format.fprintf fmt "Lambda (%a: %a => %a)" pp_lambda_params params
        pp_effects effects pp_ast exp
  | Ast.Lambda (params, None, Some return_type, exp) ->
      Format.fprintf fmt "Lambda (%a: %a => %a)" pp_lambda_params params
        pp_ttype return_type pp_ast exp
  | Ast.Lambda (params, Some effects, Some return_type, exp) ->
      Format.fprintf fmt "Lambda (%a: %a %a => %a)" pp_lambda_params params
        pp_effects effects pp_ttype return_type pp_ast exp
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
  | Ast.Effect (name, actions, next) ->
      Format.fprintf fmt "Effect (%s, %a, %a)" name
        (Format.pp_print_list Format.pp_print_string)
        actions pp_ast next
  | Ast.Handle (exp, effect, actions) ->
      Format.fprintf fmt "Handle (%a, %s, %a)" pp_ast exp effect
        (Format.pp_print_list (pp_pair Format.pp_print_string pp_ast))
        actions

let parse_string s =
  let lexbuf = Lexing.from_string s in
  Parser.main Lexer.token lexbuf
