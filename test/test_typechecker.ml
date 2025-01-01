open Test_utils
open Liteeffects.Typecheck
open Liteeffects.Typecheck_utils
open Liteeffects.Ast

let test_int () =
  Alcotest.(check (result unit typecheck_error_testable))
    "passes int typecheck" (Ok ())
    (check (Int 1) TInt StringMap.empty)

let test_lambda () =
  Alcotest.(check (result unit typecheck_error_testable))
    "fails int typecheck"
    (Error (Expected (TInt, TLambda ([], TInt))))
    (check (Lambda ([], None, Int 1)) TInt StringMap.empty);
  Alcotest.(check (result unit typecheck_error_testable))
    "lambda type synthesizes" (Ok ())
    (check_empty (Lambda ([], None, Int 1)) (TLambda ([], TInt)));
  Alcotest.(check (result unit typecheck_error_testable))
    "lambda type synthesis fails" (Error LambdaReturnTypeMismatch)
    (let nested_lambda = Lambda ([], None, Int 1) in
     check_empty (Lambda ([], None, nested_lambda)) (TLambda ([], TInt)))

let test_app () =
  Alcotest.(check (result unit typecheck_error_testable))
    "function application w no arguments typechecks" (Ok ())
    (let env = StringMap.(empty |> add "main" (TLambda ([], TInt))) in
     check (App ("main", [])) TInt env);
  Alcotest.(check (result unit typecheck_error_testable))
    "function application return type mismatch fails"
    (Error FunctionApplicationReturnTypeMismatch)
    (let env =
       StringMap.(empty |> add "main" (TLambda ([], TLambda ([], TInt))))
     in
     check (App ("main", [])) TInt env);
  Alcotest.(check (result unit typecheck_error_testable))
    "function application w wrong args count fails"
    (Error FunctionCallArgCountMismatch)
    (let env =
       StringMap.(empty |> add "main" (TLambda ([], TLambda ([], TInt))))
     in
     check (App ("main", [ Int 1 ])) TInt env);
  Alcotest.(check (result unit typecheck_error_testable))
    "function application w wrong arg type fails"
    (Error FunctionCallArgTypeMismatch)
    (let main_type = TLambda ([ TLambda ([], TInt) ], TLambda ([], TInt)) in
     let env = StringMap.(empty |> add "main" main_type) in
     check (App ("main", [ Int 1 ])) TInt env);
  Alcotest.(check (result unit typecheck_error_testable))
    "not a function application fails" (Error (IsNotAFunction "x"))
    (let x_type = TInt in
     let env = StringMap.(empty |> add "x" x_type) in
     check (App ("x", [ Int 1 ])) TInt env)

let test_bound () =
  Alcotest.(check (result unit typecheck_error_testable))
    "const binding w no type application typechecks" (Ok ())
    (let f_type = TLambda ([ TInt ], TInt) in
     let env = StringMap.(empty |> add "f" f_type) in
     check
       (* const x = 4; f(x) where f accepts int *)
       (Bound ("x", None, Int 4, App ("f", [ Ref "x" ])))
       TInt env);
  Alcotest.(check (result unit typecheck_error_testable))
    "bound lambda type synthesizes" (Ok ())
    (check_empty
       (Bound ("f", None, Lambda ([], None, Int 1), Ref "f"))
       (TLambda ([], TInt)))

let test_ref () =
  Alcotest.(check (result unit typecheck_error_testable))
    "ref typechecks" (Ok ())
    (let x_type = TInt in
     let env = StringMap.(empty |> add "x" x_type) in
     check (Ref "x") TInt env);
  Alcotest.(check (result unit typecheck_error_testable))
    "ref typecheck fails"
    (Error (Expected (TLambda ([ TInt ], TInt), TInt)))
    (let x_type = TInt in
     let env = StringMap.(empty |> add "x" x_type) in
     check (Ref "x") (TLambda ([ TInt ], TInt)) env);
  Alcotest.(check (result unit typecheck_error_testable))
    "ref application type mismatch fails" (Error FunctionCallArgTypeMismatch)
    (* f: (() => Int) => Int *)
    (let f_type = TLambda ([ TLambda ([], TInt) ], TInt) in
     let env = StringMap.(empty |> add "f" f_type) in
     check
       (* const x = 4; f(x) *)
       (Bound ("x", None, Int 4, App ("f", [ Ref "x" ])))
       TInt env)

let test_add () =
  Alcotest.(check (result unit typecheck_error_testable))
    "addition of ints typechecks" (Ok ())
    (let env = StringMap.(empty |> add "a" TInt) in
     check (* a + 1 where a: Int *) (Add (Ref "a", Int 1)) TInt env);
  Alcotest.(check (result unit typecheck_error_testable))
    "int + lambda typecheck fails"
    (Error (Expected (TInt, TLambda ([], TInt))))
    (let env = StringMap.(empty |> add "a" (TLambda ([], TInt))) in
     check (* a + 1 where a: () => Int *) (Add (Ref "a", Int 1)) TInt env)

let test_suite =
  [
    Alcotest.test_case "Int" `Quick test_int;
    Alcotest.test_case "Lambda" `Quick test_lambda;
    Alcotest.test_case "App" `Quick test_app;
    Alcotest.test_case "Bound" `Quick test_bound;
    Alcotest.test_case "Ref" `Quick test_ref;
    Alcotest.test_case "Add" `Quick test_add;
  ]
