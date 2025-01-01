open Alcotest
open Test_utils
open Liteeffects.Typecheck
open Liteeffects.Typecheck_utils
open Liteeffects.Ast

let test_int () =
  Alcotest.(check (result unit typecheck_error_testable))
    "passes int typecheck" (Ok ())
    (check (Liteeffects.Ast.Int 1) Liteeffects.Ast.TInt StringMap.empty)

let test_lambda () =
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

let test_app () =
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
     check (Liteeffects.Ast.App ("x", [ Int 1 ])) Liteeffects.Ast.TInt env)

let test_bound () =
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
    "bound lambda type synthesizes" (Ok ())
    (check_empty
       (Bound ("f", None, Lambda ([], None, Int 1), Ref "f"))
       (Liteeffects.Ast.TLambda ([], TInt)))

let test_ref () =
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
       Liteeffects.Ast.TInt env)

let test_add () =
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
       Liteeffects.Ast.TInt env)

let test_suite =
  [
    test_case "Int" `Quick test_int;
    test_case "Lambda" `Quick test_lambda;
    test_case "App" `Quick test_app;
    test_case "Bound" `Quick test_bound;
    test_case "Ref" `Quick test_ref;
    test_case "Add" `Quick test_add;
  ]
