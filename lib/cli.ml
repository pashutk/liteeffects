let rec ltts = function
  | Ast.Int _i -> "int"
  | Ast.Add (_a, _b) -> "add"
  | Ast.Bound (id, _typ, _value, _exp) -> "const " ^ id
  | Ast.Lambda (params, _typ, exp) ->
      "lambda ("
      ^ String.concat ", "
          (List.map (fun x -> match x with name, _ttype -> name) params)
      ^ ") => " ^ ltts exp
  | Ast.Ref name -> "ref " ^ name
  | Ast.Mult (left, right) -> "mult " ^ ltts left ^ " * " ^ ltts right
  | Ast.App (id, _args) -> "app " ^ id
  | Ast.Perform (effect, _action, _exp) -> "perform " ^ effect
  | Ast.Effect (name, _actions) -> "effect " ^ name
  | Ast.Handle (_exp, effect, _actions) -> "handle " ^ effect

let lbts = function
  | Parser.ARROW -> "arrow"
  | Parser.ID name -> "id " ^ name
  | _ -> "other"

let parse s =
  let lexbuf = Lexing.from_string s in
  Parser.main Lexer.token lexbuf

let main () =
  (* let lexbuf = Lexing.from_channel stdin in *)
  let lexbuf =
    Lexing.from_string
      "const addAndLet = () => {\n\
       const x = 5;\n\
       const y = 3;\n\
       const result = x + y * 2;\n\
       result\n\
       };\n\
       addAndLet()"
  in
  let ast = lexbuf |> Parser.main Lexer.token in
  let typecheck_result = Typecheck.check_main ast in
  match typecheck_result with
  | Ok _ -> ast |> Interpret.interpret_start |> Int.to_string |> print_endline
  | Error err -> Typecheck_utils.pp_type_error Format.std_formatter err
