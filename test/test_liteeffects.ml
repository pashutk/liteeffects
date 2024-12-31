open Liteeffects.Cli
open Liteeffects.Ast_utils
open Liteeffects.Typecheck
open Liteeffects.Typecheck_utils

let ast_testable = Alcotest.testable pp_ast ( = )
let typecheck_error_testable = Alcotest.testable pp_type_error ( = )
let test_int () = Alcotest.check ast_testable "parse '1'" (Int 1) (parse "1")

let test_add () =
  Alcotest.check ast_testable "parse '1 + 2'"
    (Add (Int 1, Int 2))
    (parse "1 + 2")

let test_mult () =
  Alcotest.check ast_testable "parse '1 * 2'"
    (Mult (Int 1, Int 2))
    (parse "1 * 2");
  Alcotest.check ast_testable "parse '1 + 1 * 2'"
    (Add (Int 1, Mult (Int 1, Int 2)))
    (parse "1 + 1 * 2");
  Alcotest.check ast_testable "parse '2 * 2 + 2'"
    (Add (Mult (Int 2, Int 2), Int 2))
    (parse "2 * 2 + 2")

let test_const () =
  Alcotest.check ast_testable "parse const binding"
    (Bound ("x", None, Int 1, Ref "x"))
    (parse "const x = 1; x");
  Alcotest.check ast_testable "parse multiple const bindings"
    (Bound ("x", None, Int 1, Bound ("y", None, Int 2, Add (Ref "x", Ref "y"))))
    (parse "const x = 1; const y = 2; x + y");
  Alcotest.check ast_testable "parse lambda const binding"
    (Bound ("f", None, Lambda ([], None, Int 1), Ref "f"))
    (parse "const f = () => { 1 }; f");
  Alcotest.check ast_testable "parse with no semicolon"
    (Bound ("f", None, Lambda ([], None, Int 1), App ("f", [])))
    (parse "const f = () => { 1 }\nf()")

let test_lambda () =
  Alcotest.check ast_testable "parse function w no args"
    (Lambda ([], None, Int 1))
    (parse "() => { 1 }");
  Alcotest.check ast_testable "parse function w one arg"
    (Lambda ([ ("a", Liteeffects.Ast.TInt) ], None, Int 1))
    (parse "(a: Int) => { 1 }");
  Alcotest.check ast_testable "parse function w one arg and return type"
    (Lambda ([ ("a", Liteeffects.Ast.TInt) ], Some TInt, Int 1))
    (parse "(a: Int): Int => { 1 }");
  Alcotest.check ast_testable "parse function w lambda param"
    (Lambda
       ( [
           ( "f",
             Liteeffects.Ast.TLambda
               ([ Liteeffects.Ast.TInt ], Liteeffects.Ast.TInt) );
         ],
         Some TInt,
         Int 1 ))
    (parse "(f: (Int) => Int): Int => { 1 }");
  Alcotest.check ast_testable "parse function w multiple args"
    (Lambda
       ( [
           ("a", Liteeffects.Ast.TInt);
           ("b", Liteeffects.Ast.TInt);
           ("c", Liteeffects.Ast.TInt);
         ],
         None,
         Int 1 ))
    (parse "(a: Int, b: Int, c: Int) => { 1 }");
  Alcotest.check ast_testable "parse function w const bindings"
    (Lambda
       ( [],
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
    (parse
       "() => {\n\
        const x = 5;\n\
        const y = 3;\n\
        const result = x + y * 2;\n\
        result\n\
        }");
  Alcotest.check ast_testable "parse lambda with no braces"
    (Lambda ([], None, Int 1))
    (parse "() => 1 ")

let test_app () =
  Alcotest.check ast_testable "parse simple application"
    (App ("withArgs", [ Int 2 ]))
    (parse "withArgs(2)");
  Alcotest.check ast_testable "parse application with multiple args"
    (App ("withArgs", [ Int 2; Int 3 ]))
    (parse "withArgs(2, 3)");
  Alcotest.check ast_testable "parse lambda with application"
    (Lambda ([], None, Add (Int 1, App ("withArgs", [ Int 2 ]))))
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
           ("pi", Lambda ([], None, Int 1));
           ( "sin",
             Lambda ([ ("a", Liteeffects.Ast.TInt) ], None, Add (Ref "a", Int 1))
           );
         ] ))
    (parse
       "handle effectfulComputation(1) with Math { pi: () => { 1 }, sin: (a: \
        Int) => { a + 1 } }")

let test_typecheck_basic () =
  Alcotest.(check (result unit typecheck_error_testable))
    "passes int typecheck" (Ok ())
    (check (Liteeffects.Ast.Int 1) Liteeffects.Ast.TInt StringMap.empty);
  Alcotest.(check (result unit typecheck_error_testable))
    "fails int typecheck"
    (Error
       (Expected
          ( Liteeffects.Ast.TInt,
            Liteeffects.Ast.TLambda ([], Liteeffects.Ast.TInt) )))
    (check
       (Liteeffects.Ast.Lambda ([], None, Int 1))
       Liteeffects.Ast.TInt StringMap.empty);
  Alcotest.(check (result unit typecheck_error_testable))
    "function application w no arguments typechecks" (Ok ())
    (let env =
       StringMap.(
         empty
         |> add "main" (Liteeffects.Ast.TLambda ([], Liteeffects.Ast.TInt)))
     in
     check (Liteeffects.Ast.App ("main", [])) Liteeffects.Ast.TInt env);
  Alcotest.(check (result unit typecheck_error_testable))
    "function application return type mismatch fails"
    (Error FunctionApplicationReturnTypeMismatch)
    (let env =
       StringMap.(
         empty
         |> add "main"
              (Liteeffects.Ast.TLambda ([], Liteeffects.Ast.TLambda ([], TInt))))
     in
     check (Liteeffects.Ast.App ("main", [])) Liteeffects.Ast.TInt env);
  Alcotest.(check (result unit typecheck_error_testable))
    "function application w wrong args count fails"
    (Error FunctionCallArgCountMismatch)
    (let env =
       StringMap.(
         empty
         |> add "main"
              (Liteeffects.Ast.TLambda ([], Liteeffects.Ast.TLambda ([], TInt))))
     in
     check (Liteeffects.Ast.App ("main", [ Int 1 ])) Liteeffects.Ast.TInt env);
  Alcotest.(check (result unit typecheck_error_testable))
    "function application w wrong arg type fails"
    (Error FunctionCallArgTypeMismatch)
    (let main_type =
       Liteeffects.Ast.TLambda
         ( [ Liteeffects.Ast.TLambda ([], TInt) ],
           Liteeffects.Ast.TLambda ([], TInt) )
     in
     let env = StringMap.(empty |> add "main" main_type) in
     check (Liteeffects.Ast.App ("main", [ Int 1 ])) Liteeffects.Ast.TInt env);
  Alcotest.(check (result unit typecheck_error_testable))
    "not a function application fails" (Error (IsNotAFunction "x"))
    (let x_type = Liteeffects.Ast.TInt in
     let env = StringMap.(empty |> add "x" x_type) in
     check (Liteeffects.Ast.App ("x", [ Int 1 ])) Liteeffects.Ast.TInt env);
  Alcotest.(check (result unit typecheck_error_testable))
    "const binding w no type application typechecks" (Ok ())
    (let f_type = Liteeffects.Ast.TLambda ([ TInt ], TInt) in
     let env = StringMap.(empty |> add "f" f_type) in
     check
       (* const x = 4; f(x) where f accepts int *)
       (Liteeffects.Ast.Bound
          ( "x",
            None,
            Int 4,
            Liteeffects.Ast.App ("f", [ Liteeffects.Ast.Ref "x" ]) ))
       Liteeffects.Ast.TInt env);
  Alcotest.(check (result unit typecheck_error_testable))
    "ref typechecks" (Ok ())
    (let x_type = Liteeffects.Ast.TInt in
     let env = StringMap.(empty |> add "x" x_type) in
     check (Liteeffects.Ast.Ref "x") Liteeffects.Ast.TInt env);
  Alcotest.(check (result unit typecheck_error_testable))
    "ref typecheck fails"
    (Error (Expected (Liteeffects.Ast.TLambda ([ TInt ], TInt), TInt)))
    (let x_type = Liteeffects.Ast.TInt in
     let env = StringMap.(empty |> add "x" x_type) in
     check (Liteeffects.Ast.Ref "x")
       (Liteeffects.Ast.TLambda ([ TInt ], TInt))
       env);
  Alcotest.(check (result unit typecheck_error_testable))
    "ref application type mismatch fails" (Error FunctionCallArgTypeMismatch)
    (* f: (() => Int) => Int *)
    (let f_type = Liteeffects.Ast.TLambda ([ TLambda ([], TInt) ], TInt) in
     let env = StringMap.(empty |> add "f" f_type) in
     check
       (* const x = 4; f(x) *)
       (Liteeffects.Ast.Bound
          ( "x",
            None,
            Int 4,
            Liteeffects.Ast.App ("f", [ Liteeffects.Ast.Ref "x" ]) ))
       Liteeffects.Ast.TInt env);
  Alcotest.(check (result unit typecheck_error_testable))
    "addition of ints typechecks" (Ok ())
    (let env = StringMap.(empty |> add "a" Liteeffects.Ast.TInt) in
     check
       (* a + 1 where a: Int *)
       (Liteeffects.Ast.Add (Ref "a", Int 1))
       Liteeffects.Ast.TInt env);
  Alcotest.(check (result unit typecheck_error_testable))
    "int + lambda typecheck fails"
    (Error
       (Expected
          ( Liteeffects.Ast.TInt,
            Liteeffects.Ast.TLambda ([], Liteeffects.Ast.TInt) )))
    (let env =
       StringMap.(
         empty |> add "a" (Liteeffects.Ast.TLambda ([], Liteeffects.Ast.TInt)))
     in
     check
       (* a + 1 where a: () => Int *)
       (Liteeffects.Ast.Add (Ref "a", Int 1))
       Liteeffects.Ast.TInt env);
  Alcotest.(check (result unit typecheck_error_testable))
    "lambda type synthesizes" (Ok ())
    (check_empty
       (Lambda ([], None, Liteeffects.Ast.Int 1))
       (Liteeffects.Ast.TLambda ([], TInt)));
  Alcotest.(check (result unit typecheck_error_testable))
    "lambda type synthesis fails"
    (Error Liteeffects.Typecheck_utils.LambdaReturnTypeMismatch)
    (let nested_lambda = Liteeffects.Ast.Lambda ([], None, Int 1) in
     check_empty
       (Lambda ([], None, nested_lambda))
       (Liteeffects.Ast.TLambda ([], TInt)))

let test_interpret () =
  Alcotest.(check int)
    "interprets int" 1
    (Liteeffects.Interpret.interpret_empty (Int 1));
  Alcotest.(check int)
    "interprets add" 3
    (Liteeffects.Interpret.interpret_empty (Add (Int 1, Int 2)));
  Alcotest.(check int)
    "interprets mult" 6
    (Liteeffects.Interpret.interpret_empty (Mult (Int 3, Int 2)));
  Alcotest.(check int)
    "addition of ref and int interprets" 5
    (Liteeffects.Interpret.interpret
       (Add (Ref "a", Int 2))
       (let a = Liteeffects.Ast.Int 3 in
        Liteeffects.Interpret.StringMap.(empty |> add "a" a)));
  Alcotest.(check int)
    "addition of application and int interprets" 6
    (Liteeffects.Interpret.interpret
       (* f(1) + 2 *)
       (Add (App ("f", [ Int 1 ]), Int 2))
       (let f =
          (* a => a + 3 *)
          Liteeffects.Ast.Lambda
            ([ ("a", Liteeffects.Ast.TInt) ], None, Add (Ref "a", Int 3))
        in
        Liteeffects.Interpret.StringMap.(empty |> add "f" f)));
  Alcotest.check_raises "interpreting addition of anything else fails"
    (Failure "Not implemented or the ast hasn't been typechecked") (fun () ->
      ignore
        (Liteeffects.Interpret.interpret_empty
           (Add (Lambda ([], None, Int 1), Int 2))));
  Alcotest.(check int)
    "binding use in addition interprets" 3
    (Liteeffects.Interpret.interpret_empty
       (Bound ("a", None, Int 1, Add (Ref "a", Int 2))));
  Alcotest.(check int)
    "lambda binding interprets" 1
    (Liteeffects.Interpret.interpret_empty
       (Bound
          ( "f",
            None,
            Lambda ([ ("a", TInt) ], None, Ref "a"),
            App ("f", [ Int 1 ]) )));
  Alcotest.(check int)
    "application of lambda interprets" 2
    (Liteeffects.Interpret.interpret_empty
       (* const app = (f, value) => f(value); app((a) => a + 1, 1) *)
       ((* (Int) => Int *)
        let f_param_type =
          Liteeffects.Ast.TLambda
            ([ Liteeffects.Ast.TInt ], Liteeffects.Ast.TInt)
        in
        (* (f: f_param_type, value: Int) => f(value) *)
        let app =
          Liteeffects.Ast.Lambda
            ( [ ("f", f_param_type); ("value", Liteeffects.Ast.TInt) ],
              None,
              App ("f", [ Ref "value" ]) )
        in
        let inc =
          Liteeffects.Ast.Lambda
            ([ ("a", Liteeffects.Ast.TInt) ], None, Add (Ref "a", Int 1))
        in
        let result =
          Liteeffects.Ast.App ("app", [ inc; Liteeffects.Ast.Int 1 ])
        in
        Bound ("app", None, app, result)));
  Alcotest.(check int)
    "application of bound lambda interprets" 2
    (Liteeffects.Interpret.interpret_empty
       (* const inc = (a) => a + 1; const app = (f, value) => f(value); app(inc, 1) *)
       (let inc =
          Liteeffects.Ast.Lambda
            ([ ("a", Liteeffects.Ast.TInt) ], None, Add (Ref "a", Int 1))
        in
        let f_param_type =
          Liteeffects.Ast.TLambda
            ([ Liteeffects.Ast.TInt ], Liteeffects.Ast.TInt)
        in
        let app =
          Liteeffects.Ast.Lambda
            ( [ ("f", f_param_type); ("value", Liteeffects.Ast.TInt) ],
              None,
              App ("f", [ Ref "value" ]) )
        in
        let result =
          Liteeffects.Ast.App ("app", [ Ref "inc"; Liteeffects.Ast.Int 1 ])
        in
        Bound ("inc", None, inc, Bound ("app", None, app, result))))

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
      ("typecheck", [ test_case "Basic" `Quick test_typecheck_basic ]);
      ("interpret", [ test_case "Basic" `Quick test_interpret ]);
    ]
