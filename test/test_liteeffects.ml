open Liteeffects.Cli

let pp_pair pp_first pp_second fmt (first, second) =
  Format.fprintf fmt "(%a, %a)" pp_first first pp_second second

let rec pp_ast fmt = function
  | Liteeffects.Ast.Int i -> Format.fprintf fmt "Int %d" i
  | Liteeffects.Ast.Ref name -> Format.fprintf fmt "Ref %s" name
  | Liteeffects.Ast.Add (a, b) ->
      Format.fprintf fmt "Add (%a, %a)" pp_ast a pp_ast b
  | Liteeffects.Ast.Mult (a, b) ->
      Format.fprintf fmt "Mult (%a, %a)" pp_ast a pp_ast b
  | Liteeffects.Ast.Lambda (params, exp) ->
      Format.fprintf fmt "Lambda (%a, %a)"
        (Format.pp_print_list Format.pp_print_string)
        params pp_ast exp
  | Liteeffects.Ast.App (id, args) ->
      Format.fprintf fmt "App (%s, %a)" id (Format.pp_print_list pp_ast) args
  | Liteeffects.Ast.Perform (effect, action, args) ->
      Format.fprintf fmt "Perform (%s, %s, %a)" effect action
        (Format.pp_print_list pp_ast)
        args
  | Liteeffects.Ast.Bound (name, value, exp) ->
      Format.fprintf fmt "Bound (%s, %a, %a)" name pp_ast value pp_ast exp
  | Liteeffects.Ast.Effect (name, actions) ->
      Format.fprintf fmt "Effect (%s, %a)" name
        (Format.pp_print_list Format.pp_print_string)
        actions
  | Liteeffects.Ast.Handle (exp, effect, actions) ->
      Format.fprintf fmt "Handle (%a, %s, %a)" pp_ast exp effect
        (Format.pp_print_list (pp_pair Format.pp_print_string pp_ast))
        actions

let ast_testable = Alcotest.testable pp_ast ( = )
let test_int () = Alcotest.check ast_testable "parse '1'" (Int 1) (parse "1")

let test_add () =
  Alcotest.check ast_testable "parse '1 + 2'"
    (Add (Int 1, Int 2))
    (parse "1 + 2")

let test_mult () =
  Alcotest.check ast_testable "parse '1 * 2'"
    (Mult (Int 1, Int 2))
    (parse "1 * 2")

let test_const () =
  Alcotest.check ast_testable "parse const binding"
    (Bound ("x", Int 1, Ref "x"))
    (parse "const x = 1; x");
  Alcotest.check ast_testable "parse multiple const bindings"
    (Bound ("x", Int 1, Bound ("y", Int 2, Add (Ref "x", Ref "y"))))
    (parse "const x = 1; const y = 2; x + y");
  Alcotest.check ast_testable "parse lambda const binding"
    (Bound ("f", Lambda ([], Int 1), Ref "f"))
    (parse "const f = () => { 1 }; f");
  Alcotest.check ast_testable "parse with no semicolon"
    (Bound ("f", Lambda ([], Int 1), App ("f", [])))
    (parse "const f = () => { 1 }\nf()")

let test_lambda () =
  Alcotest.check ast_testable "parse function w no args"
    (Lambda ([], Int 1))
    (parse "() => { 1 }");
  Alcotest.check ast_testable "parse function w one arg"
    (Lambda ([ "a" ], Int 1))
    (parse "(a) => { 1 }");
  Alcotest.check ast_testable "parse function w multiple args"
    (Lambda ([ "a"; "b"; "c" ], Int 1))
    (parse "(a, b, c) => { 1 }");
  Alcotest.check ast_testable "parse function w const bindings"
    (Lambda
       ( [],
         Bound
           ( "x",
             Int 5,
             Bound
               ( "y",
                 Int 3,
                 Bound
                   ("result", Add (Ref "x", Mult (Ref "y", Int 2)), Ref "result")
               ) ) ))
    (parse
       "() => {\n\
        const x = 5;\n\
        const y = 3;\n\
        const result = x + y * 2;\n\
        result\n\
        }");
  Alcotest.check ast_testable "parse lambda with no braces"
    (Lambda ([], Int 1))
    (parse "() => 1 ")

let test_app () =
  Alcotest.check ast_testable "parse simple application"
    (App ("withArgs", [ Int 2 ]))
    (parse "withArgs(2)");
  Alcotest.check ast_testable "parse application with multiple args"
    (App ("withArgs", [ Int 2; Int 3 ]))
    (parse "withArgs(2, 3)");
  Alcotest.check ast_testable "parse lambda with application"
    (Lambda ([], Add (Int 1, App ("withArgs", [ Int 2 ]))))
    (parse "() => { 1 + withArgs(2) }")

let test_perform () =
  Alcotest.check ast_testable "parse simple perform call"
    (Perform ("Effect", "action", [ Int 2 ]))
    (parse "perform Effect.action(2)");
  Alcotest.check ast_testable "parse perform call with multiple args"
    (Perform ("Effect", "action", [ Int 2; Ref "a" ]))
    (parse "perform Effect.action(2, a)")

let test_effect () =
  Alcotest.check ast_testable "parse effect definition"
    (Effect ("Math", [ "pi"; "sin" ]))
    (parse "effect Math {pi, sin}")

let test_handle () =
  Alcotest.check ast_testable "parse handle definition"
    (Handle
       ( App ("effectfulComputation", [ Int 1 ]),
         "Math",
         [
           ("pi", Lambda ([], Int 1));
           ("sin", Lambda ([ "a" ], Add (Ref "a", Int 1)));
         ] ))
    (parse
       "handle effectfulComputation(1) with Math { pi: () => { 1 }, sin: (a) \
        => { a + 1 } }")

let () =
  let open Alcotest in
  run "Parser"
    [
      ( "ast",
        [
          test_case "Int" `Quick test_int;
          test_case "Add" `Quick test_add;
          test_case "Mult" `Quick test_mult;
          test_case "Const" `Quick test_const;
          test_case "Lambda" `Quick test_lambda;
          test_case "App" `Quick test_app;
          test_case "Perform" `Quick test_perform;
          test_case "Effect" `Quick test_effect;
          test_case "Handle" `Quick test_handle;
        ] );
    ]
