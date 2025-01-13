open Test_utils
open Liteeffects.Typecheck
open Liteeffects.Typecheck_utils
open Liteeffects.Ast
open Liteeffects.TEnv

let test_int () =
  Alcotest.(check (result unit typecheck_error_testable))
    "passes int typecheck" (Ok ())
    (check_empty (Int 1) TInt [])

let test_lambda () =
  Alcotest.(check (result unit typecheck_error_testable))
    "fails int typecheck"
    (Error (Expected (TInt, TLambda ([], None, TInt))))
    (check_empty (Lambda ([], None, None, Int 1)) TInt []);
  Alcotest.(check (result unit typecheck_error_testable))
    "lambda type synthesizes" (Ok ())
    (check_empty (Lambda ([], None, None, Int 1)) (TLambda ([], None, TInt)) []);
  Alcotest.(check (result unit typecheck_error_testable))
    "lambda type synthesis fails" (Error LambdaReturnTypeMismatch)
    (let nested_lambda = Lambda ([], None, None, Int 1) in
     check_empty
       (Lambda ([], None, None, nested_lambda))
       (TLambda ([], None, TInt))
       [])

let test_app () =
  Alcotest.(check (result unit typecheck_error_testable))
    "function application w no arguments typechecks" (Ok ())
    (let env = empty |> add_binding "main" (TLambda ([], None, TInt)) in
     check (App ("main", [])) TInt [] env);
  Alcotest.(check (result unit typecheck_error_testable))
    "function application return type mismatch fails"
    (Error FunctionApplicationReturnTypeMismatch)
    (let env =
       empty
       |> add_binding "main" (TLambda ([], None, TLambda ([], None, TInt)))
     in
     check (App ("main", [])) TInt [] env);
  Alcotest.(check (result unit typecheck_error_testable))
    "function application w wrong args count fails"
    (Error FunctionCallArgCountMismatch)
    (let env =
       empty
       |> add_binding "main" (TLambda ([], None, TLambda ([], None, TInt)))
     in
     check (App ("main", [ Int 1 ])) TInt [] env);
  Alcotest.(check (result unit typecheck_error_testable))
    "function application w wrong arg type fails"
    (Error FunctionCallArgTypeMismatch)
    (let main_type =
       TLambda ([ TLambda ([], None, TInt) ], None, TLambda ([], None, TInt))
     in
     let env = empty |> add_binding "main" main_type in
     check (App ("main", [ Int 1 ])) TInt [] env);
  Alcotest.(check (result unit typecheck_error_testable))
    "not a function application fails" (Error (IsNotAFunction "x"))
    (let x_type = TInt in
     let env = empty |> add_binding "x" x_type in
     check (App ("x", [ Int 1 ])) TInt [] env)

let test_bound () =
  Alcotest.(check (result unit typecheck_error_testable))
    "const binding w no type application typechecks" (Ok ())
    (let f_type = TLambda ([ TInt ], None, TInt) in
     let env = empty |> add_binding "f" f_type in
     check
       (* const x = 4; f(x) where f accepts int *)
       (Bound ("x", None, Int 4, App ("f", [ Ref "x" ])))
       TInt [] env);
  Alcotest.(check (result unit typecheck_error_testable))
    "bound lambda type synthesizes" (Ok ())
    (check_empty
       (Bound ("f", None, Lambda ([], None, None, Int 1), Ref "f"))
       (TLambda ([], None, TInt))
       [])

let test_ref () =
  Alcotest.(check (result unit typecheck_error_testable))
    "ref typechecks" (Ok ())
    (let x_type = TInt in
     let env = empty |> add_binding "x" x_type in
     check (Ref "x") TInt [] env);
  Alcotest.(check (result unit typecheck_error_testable))
    "ref typecheck fails"
    (Error (Expected (TLambda ([ TInt ], None, TInt), TInt)))
    (let x_type = TInt in
     let env = empty |> add_binding "x" x_type in
     check (Ref "x") (TLambda ([ TInt ], None, TInt)) [] env);
  Alcotest.(check (result unit typecheck_error_testable))
    "ref application type mismatch fails" (Error FunctionCallArgTypeMismatch)
    (* f: (() => Int) => Int *)
    (let f_type = TLambda ([ TLambda ([], None, TInt) ], None, TInt) in
     let env = empty |> add_binding "f" f_type in
     check
       (* const x = 4; f(x) *)
       (Bound ("x", None, Int 4, App ("f", [ Ref "x" ])))
       TInt [] env)

let test_add () =
  Alcotest.(check (result unit typecheck_error_testable))
    "addition of ints typechecks" (Ok ())
    (let env = empty |> add_binding "a" TInt in
     check (* a + 1 where a: Int *) (Add (Ref "a", Int 1)) TInt [] env);
  Alcotest.(check (result unit typecheck_error_testable))
    "int + lambda typecheck fails"
    (Error (Expected (TInt, TLambda ([], None, TInt))))
    (let env = empty |> add_binding "a" (TLambda ([], None, TInt)) in
     check (* a + 1 where a: () => Int *) (Add (Ref "a", Int 1)) TInt [] env)

let test_handle () =
  Alcotest.(check (result unit typecheck_error_testable))
    "handling an exp with unknown effect fails" (Error (UnknownEffect "Math"))
    (check_main (Handle (Int 1, "Math", [])));
  Alcotest.(check (result unit typecheck_error_testable))
    "handling an exp with known effect typechecks" (Ok ())
    (let env = empty |> add_effect "Math" StringMap.empty in
     check (Handle (Int 1, "Math", [])) TInt [] env);
  Alcotest.(check (result unit typecheck_error_testable))
    "handling non effectful expression typechecks" (Ok ())
    (let env = empty |> add_effect "Math" StringMap.empty in
     check (Handle (Int 1, "Math", [])) TInt [] env)

let test_perform () =
  Alcotest.(check (result unit typecheck_error_testable))
    "performing unknown effect fails" (Error (UnknownEffect "Console"))
    (check_main (Perform ("Console", "log", [])))

let test_synthesize () =
  Alcotest.(check (result unit typecheck_error_testable))
    "performing unknown effect fails" (Error (UnknownEffect "Console"))
    (check_main (Perform ("Console", "log", [])))

let () =
  let open Alcotest in
  run "Typechecker"
    [
      ("Int", [ test_case "Int" `Quick test_int ]);
      ("Lambda", [ test_case "Lambda" `Quick test_lambda ]);
      ("App", [ test_case "App" `Quick test_app ]);
      ("Bound", [ test_case "Bound" `Quick test_bound ]);
      ("Ref", [ test_case "Ref" `Quick test_ref ]);
      ("Add", [ test_case "Add" `Quick test_add ]);
      ("Handle", [ test_case "Handle" `Quick test_handle ]);
      ("Perform", [ test_case "Perform" `Quick test_perform ]);
      ("Sythesize", [ test_case "Sythesize" `Quick test_synthesize ]);
    ]
