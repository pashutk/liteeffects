open Ast_utils
module StringMap = Map.Make (String)

type env_t = Ast.typ StringMap.t

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

let pp_type_error ppf = function
  | Expected (expected, got) ->
      Format.fprintf ppf "Expected %a but got %a" pp_ttype expected pp_ttype got
  (* | Bar i -> Format.fprintf ppf "Bar %d" i *)
  | LambdaParamsCountMismatch -> Format.fprintf ppf "LambdaParamsCountMismatch"
  | LambdaReturnTypeMismatch -> Format.fprintf ppf "LambdaReturnTypeMismatch"
  | LambdaParamTypeMismatch -> Format.fprintf ppf "LambdaParamTypeMismatch"
  | UndefinedFunction name -> Format.fprintf ppf "UndefinedFunction %s" name
  | IsNotAFunction name -> Format.fprintf ppf "IsNotAFunction %s" name
  | FunctionCallArgCountMismatch ->
      Format.fprintf ppf "FunctionCallArgCountMismatch"
  | FunctionCallArgTypeMismatch ->
      Format.fprintf ppf "FunctionCallArgTypeMismatch"
  | FunctionApplicationReturnTypeMismatch ->
      Format.fprintf ppf "FunctionApplicationReturnTypeMismatch"
  | UndefinedVariable name -> Format.fprintf ppf "UndefinedVariable %s" name
  | Unknown -> Format.fprintf ppf "Unknown"
