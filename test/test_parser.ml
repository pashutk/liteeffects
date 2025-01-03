open Test_utils
open Liteeffects.Ast_utils

let test_int () =
  Alcotest.check ast_testable "parse '1'" (Int 1) (parse_string "1")

let test_add () =
  Alcotest.check ast_testable "parse '1 + 2'"
    (Add (Int 1, Int 2))
    (parse_string "1 + 2")

let test_mult () =
  Alcotest.check ast_testable "parse '1 * 2'"
    (Mult (Int 1, Int 2))
    (parse_string "1 * 2");
  Alcotest.check ast_testable "parse '1 + 1 * 2'"
    (Add (Int 1, Mult (Int 1, Int 2)))
    (parse_string "1 + 1 * 2");
  Alcotest.check ast_testable "parse '2 * 2 + 2'"
    (Add (Mult (Int 2, Int 2), Int 2))
    (parse_string "2 * 2 + 2")

let test_const () =
  Alcotest.check ast_testable "parse const binding"
    (Bound ("x", None, Int 1, Ref "x"))
    (parse_string "const x = 1; x");
  Alcotest.check ast_testable "parse multiple const bindings"
    (Bound ("x", None, Int 1, Bound ("y", None, Int 2, Add (Ref "x", Ref "y"))))
    (parse_string "const x = 1; const y = 2; x + y");
  Alcotest.check ast_testable "parse lambda const binding"
    (Bound ("f", None, Lambda ([], None, None, Int 1), Ref "f"))
    (parse_string "const f = () => { 1 }; f");
  Alcotest.check ast_testable "parse with no semicolon"
    (Bound ("f", None, Lambda ([], None, None, Int 1), App ("f", [])))
    (parse_string "const f = () => { 1 }\nf()")

let test_lambda () =
  Alcotest.check ast_testable "parse function w no args"
    (Lambda ([], None, None, Int 1))
    (parse_string "() => { 1 }");
  Alcotest.check ast_testable "parse function w one arg"
    (Lambda ([ ("a", TInt) ], None, None, Int 1))
    (parse_string "(a: Int) => { 1 }");
  Alcotest.check ast_testable "parse function w one arg and return type"
    (Lambda ([ ("a", TInt) ], None, Some TInt, Int 1))
    (parse_string "(a: Int): Int => { 1 }");
  Alcotest.check ast_testable "parse function w lambda param"
    (Lambda ([ ("f", TLambda ([ TInt ], None, TInt)) ], None, Some TInt, Int 1))
    (parse_string "(f: (Int) => Int): Int => { 1 }");
  Alcotest.check ast_testable "parse function w multiple args"
    (Lambda ([ ("a", TInt); ("b", TInt); ("c", TInt) ], None, None, Int 1))
    (parse_string "(a: Int, b: Int, c: Int) => { 1 }");
  Alcotest.check ast_testable "parse function w const bindings"
    (Lambda
       ( [],
         None,
         None,
         Bound
           ( "x",
             None,
             Int 5,
             Bound
               ( "y",
                 None,
                 Int 3,
                 Bound
                   ( "result",
                     None,
                     Add (Ref "x", Mult (Ref "y", Int 2)),
                     Ref "result" ) ) ) ))
    (parse_string
       "() => {\n\
        const x = 5;\n\
        const y = 3;\n\
        const result = x + y * 2;\n\
        result\n\
        }");
  Alcotest.check ast_testable "parse lambda with no braces"
    (Lambda ([], None, None, Int 1))
    (parse_string "() => 1 ");
  Alcotest.check ast_testable "parse lambda w 1 effect and no return type"
    (Lambda ([], Some [ "Console" ], None, Int 1))
    (parse_string "(): <Console> => { 1 }");
  Alcotest.check ast_testable "parse lambda w 1 effect and return type"
    (Lambda ([], Some [ "Console" ], None, Int 1))
    (parse_string "(): <Console> => { 1 }");
  Alcotest.check ast_testable "parse lambda w many effects"
    (Lambda ([], Some [ "Console"; "Env" ], None, Int 1))
    (parse_string "(): <Console, Env> => { 1 }")

let test_app () =
  Alcotest.check ast_testable "parse simple application"
    (App ("withArgs", [ Int 2 ]))
    (parse_string "withArgs(2)");
  Alcotest.check ast_testable "parse application with multiple args"
    (App ("withArgs", [ Int 2; Int 3 ]))
    (parse_string "withArgs(2, 3)");
  Alcotest.check ast_testable "parse lambda with application"
    (Lambda ([], None, None, Add (Int 1, App ("withArgs", [ Int 2 ]))))
    (parse_string "() => { 1 + withArgs(2) }")

let test_perform () =
  Alcotest.check ast_testable "parse simple perform call"
    (Perform ("Effect", "action", [ Int 2 ]))
    (parse_string "perform Effect.action(2)");
  Alcotest.check ast_testable "parse perform call with multiple args"
    (Perform ("Effect", "action", [ Int 2; Ref "a" ]))
    (parse_string "perform Effect.action(2, a)")

let test_effect () =
  Alcotest.check ast_testable "parse effect definition"
    (Effect ("Math", [ "pi"; "sin" ], Int 1))
    (parse_string "effect Math {pi, sin}; 1")

let test_handle () =
  Alcotest.check ast_testable "parse handle definition"
    (Handle
       ( App ("effectfulComputation", [ Int 1 ]),
         "Math",
         [
           ("pi", Lambda ([], None, None, Int 1));
           ("sin", Lambda ([ ("a", TInt) ], None, None, Add (Ref "a", Int 1)));
         ] ))
    (parse_string
       "handle effectfulComputation(1) with Math { pi: () => { 1 }, sin: (a: \
        Int) => { a + 1 } }")

let test_suite =
  [
    Alcotest.test_case "Int" `Quick test_int;
    Alcotest.test_case "Add" `Quick test_add;
    Alcotest.test_case "Mult" `Quick test_mult;
    Alcotest.test_case "Const" `Quick test_const;
    Alcotest.test_case "Lambda" `Quick test_lambda;
    Alcotest.test_case "App" `Quick test_app;
    Alcotest.test_case "Perform" `Quick test_perform;
    Alcotest.test_case "Effect" `Quick test_effect;
    Alcotest.test_case "Handle" `Quick test_handle;
  ]
