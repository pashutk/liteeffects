let rec ltts = function
  | Ast.Int _i -> "int"
  | Ast.Add (_a, _b) -> "add"
  | Ast.Bound (id, _value, _exp) -> "const " ^ id
  | Ast.Lambda (params, exp) ->
      "lambda (" ^ String.concat ", " params ^ ") => " ^ ltts exp
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
  (* let lexbuf = Lexing.from_string "const x = 1; x" in *)
  (* print_endline (lbts (Lexer.token lexbuf)) *)
  let b = Parser.main Lexer.token lexbuf in

  print_endline (ltts b)
