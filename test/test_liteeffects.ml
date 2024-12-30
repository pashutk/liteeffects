open Liteeffects.Cli

let pp_pair pp_first pp_second fmt (first, second) =
  Format.fprintf fmt "(%a, %a)" pp_first first pp_second second

let rec pp_ttype fmt = function
  | Liteeffects.Ast.TInt -> Format.fprintf fmt "Int"
  | Liteeffects.Ast.TLambda (params, result) ->
      Format.fprintf fmt "(%a) => %a"
        (Format.pp_print_list pp_ttype)
        params pp_ttype result

let pp_lambda_param fmt (name, ttype) =
  Format.fprintf fmt "%s: %a" name pp_ttype ttype

let pp_lambda_params fmt params =
  Format.fprintf fmt "(%a)" (Format.pp_print_list pp_lambda_param) params

let rec pp_ast fmt = function
  | Liteeffects.Ast.Int i -> Format.fprintf fmt "Int %d" i
  | Liteeffects.Ast.Ref name -> Format.fprintf fmt "Ref %s" name
  | Liteeffects.Ast.Add (a, b) ->
      Format.fprintf fmt "Add (%a, %a)" pp_ast a pp_ast b
  | Liteeffects.Ast.Mult (a, b) ->
      Format.fprintf fmt "Mult (%a, %a)" pp_ast a pp_ast b
  | Liteeffects.Ast.Lambda (params, return_type, exp) -> (
      match return_type with
      | None ->
          Format.fprintf fmt "Lambda (%a => %a)" pp_lambda_params params pp_ast
            exp
      | Some ttype ->
          Format.fprintf fmt "Lambda (%a: %a => %a)" pp_lambda_params params
            pp_ttype ttype pp_ast exp)
  | Liteeffects.Ast.App (id, args) ->
      Format.fprintf fmt "App (%s, %a)" id (Format.pp_print_list pp_ast) args
  | Liteeffects.Ast.Perform (effect, action, args) ->
      Format.fprintf fmt "Perform (%s, %s, %a)" effect action
        (Format.pp_print_list pp_ast)
        args
  | Liteeffects.Ast.Bound (name, None, value, exp) ->
      Format.fprintf fmt "Bound (%s, %a, %a)" name pp_ast value pp_ast exp
  | Liteeffects.Ast.Bound (name, Some typ, value, exp) ->
      Format.fprintf fmt "Bound (%s of type %a: %a, %a)" name pp_ttype typ
        pp_ast value pp_ast exp
  | Liteeffects.Ast.Effect (name, actions) ->
      Format.fprintf fmt "Effect (%s, %a)" name
        (Format.pp_print_list Format.pp_print_string)
        actions
  | Liteeffects.Ast.Handle (exp, effect, actions) ->
      Format.fprintf fmt "Handle (%a, %s, %a)" pp_ast exp effect
        (Format.pp_print_list (pp_pair Format.pp_print_string pp_ast))
        actions

let ast_testable = Alcotest.testable pp_ast ( = )

let pp_type_error fmt = function
  | Liteeffects.Typecheck.Expected (expected, got) ->
      Format.fprintf fmt "Expected %a but got %a" pp_ttype expected pp_ttype got
  | Liteeffects.Typecheck.LambdaParamTypeMismatch ->
      Format.fprintf fmt "LambdaParamTypeMismatch"
  | Liteeffects.Typecheck.LambdaParamsCountMismatch ->
      Format.fprintf fmt "LambdaParamsCountMismatch"
  | Liteeffects.Typecheck.LambdaReturnTypeMismatch ->
      Format.fprintf fmt "LambdaReturnTypeMismatch"
  | Liteeffects.Typecheck.UndefinedFunction name ->
      Format.fprintf fmt "Undefined function %s" name
  | Liteeffects.Typecheck.Unknown -> Format.fprintf fmt "Unknown"
  | Liteeffects.Typecheck.IsNotAFunction name ->
      Format.fprintf fmt "IsNotAFunction %s" name
  | Liteeffects.Typecheck.FunctionCallArgTypeMismatch ->
      Format.fprintf fmt "FunctionCallArgTypeMismatch"
  | Liteeffects.Typecheck.FunctionCallArgCountMismatch ->
      Format.fprintf fmt "FunctionCallArgCountMismatch"
  | Liteeffects.Typecheck.FunctionApplicationReturnTypeMismatch ->
      Format.fprintf fmt "FunctionApplicationReturnTypeMismatch"
  | Liteeffects.Typecheck.UndefinedVariable name ->
      Format.fprintf fmt "UndefinedVariable %s" name

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
    (Liteeffects.Typecheck.check (Liteeffects.Ast.Int 1) Liteeffects.Ast.TInt
       Liteeffects.Typecheck_utils.StringMap.empty);
  Alcotest.(check (result unit typecheck_error_testable))
    "fails int typecheck"
    (Error
       (Expected
          ( Liteeffects.Ast.TInt,
            Liteeffects.Ast.TLambda ([], Liteeffects.Ast.TInt) )))
    (Liteeffects.Typecheck.check
       (Liteeffects.Ast.Lambda ([], None, Int 1))
       Liteeffects.Ast.TInt Liteeffects.Typecheck_utils.StringMap.empty);
  Alcotest.(check (result unit typecheck_error_testable))
    "function application w no arguments typechecks" (Ok ())
    (let env =
       Liteeffects.Typecheck_utils.StringMap.(
         empty
         |> add "main" (Liteeffects.Ast.TLambda ([], Liteeffects.Ast.TInt)))
     in
     Liteeffects.Typecheck.check
       (Liteeffects.Ast.App ("main", []))
       Liteeffects.Ast.TInt env);
  Alcotest.(check (result unit typecheck_error_testable))
    "function application return type mismatch fails"
    (Error FunctionApplicationReturnTypeMismatch)
    (let env =
       Liteeffects.Typecheck_utils.StringMap.(
         empty
         |> add "main"
              (Liteeffects.Ast.TLambda ([], Liteeffects.Ast.TLambda ([], TInt))))
     in
     Liteeffects.Typecheck.check
       (Liteeffects.Ast.App ("main", []))
       Liteeffects.Ast.TInt env);
  Alcotest.(check (result unit typecheck_error_testable))
    "function application w wrong args count fails"
    (Error FunctionCallArgCountMismatch)
    (let env =
       Liteeffects.Typecheck_utils.StringMap.(
         empty
         |> add "main"
              (Liteeffects.Ast.TLambda ([], Liteeffects.Ast.TLambda ([], TInt))))
     in
     Liteeffects.Typecheck.check
       (Liteeffects.Ast.App ("main", [ Int 1 ]))
       Liteeffects.Ast.TInt env);
  Alcotest.(check (result unit typecheck_error_testable))
    "function application w wrong arg type fails"
    (Error FunctionCallArgTypeMismatch)
    (let main_type =
       Liteeffects.Ast.TLambda
         ( [ Liteeffects.Ast.TLambda ([], TInt) ],
           Liteeffects.Ast.TLambda ([], TInt) )
     in
     let env =
       Liteeffects.Typecheck_utils.StringMap.(empty |> add "main" main_type)
     in
     Liteeffects.Typecheck.check
       (Liteeffects.Ast.App ("main", [ Int 1 ]))
       Liteeffects.Ast.TInt env);
  Alcotest.(check (result unit typecheck_error_testable))
    "not a function application fails" (Error (IsNotAFunction "x"))
    (let x_type = Liteeffects.Ast.TInt in
     let env =
       Liteeffects.Typecheck_utils.StringMap.(empty |> add "x" x_type)
     in
     Liteeffects.Typecheck.check
       (Liteeffects.Ast.App ("x", [ Int 1 ]))
       Liteeffects.Ast.TInt env);
  Alcotest.(check (result unit typecheck_error_testable))
    "const binding w no type application typechecks" (Ok ())
    (let f_type = Liteeffects.Ast.TLambda ([ TInt ], TInt) in
     let env =
       Liteeffects.Typecheck_utils.StringMap.(empty |> add "f" f_type)
     in
     Liteeffects.Typecheck.check
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
     let env =
       Liteeffects.Typecheck_utils.StringMap.(empty |> add "x" x_type)
     in
     Liteeffects.Typecheck.check (Liteeffects.Ast.Ref "x") Liteeffects.Ast.TInt
       env);
  Alcotest.(check (result unit typecheck_error_testable))
    "ref typecheck fails"
    (Error (Expected (Liteeffects.Ast.TLambda ([ TInt ], TInt), TInt)))
    (let x_type = Liteeffects.Ast.TInt in
     let env =
       Liteeffects.Typecheck_utils.StringMap.(empty |> add "x" x_type)
     in
     Liteeffects.Typecheck.check (Liteeffects.Ast.Ref "x")
       (Liteeffects.Ast.TLambda ([ TInt ], TInt))
       env);
  Alcotest.(check (result unit typecheck_error_testable))
    "ref application type mismatch fails" (Error FunctionCallArgTypeMismatch)
    (* f: (() => Int) => Int *)
    (let f_type = Liteeffects.Ast.TLambda ([ TLambda ([], TInt) ], TInt) in
     let env =
       Liteeffects.Typecheck_utils.StringMap.(empty |> add "f" f_type)
     in
     Liteeffects.Typecheck.check
       (* const x = 4; f(x) *)
       (Liteeffects.Ast.Bound
          ( "x",
            None,
            Int 4,
            Liteeffects.Ast.App ("f", [ Liteeffects.Ast.Ref "x" ]) ))
       Liteeffects.Ast.TInt env);
  Alcotest.(check (result unit typecheck_error_testable))
    "addition of ints typechecks" (Ok ())
    (let env =
       Liteeffects.Typecheck_utils.StringMap.(
         empty |> add "a" Liteeffects.Ast.TInt)
     in
     Liteeffects.Typecheck.check
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
       Liteeffects.Typecheck_utils.StringMap.(
         empty |> add "a" (Liteeffects.Ast.TLambda ([], Liteeffects.Ast.TInt)))
     in
     Liteeffects.Typecheck.check
       (* a + 1 where a: () => Int *)
       (Liteeffects.Ast.Add (Ref "a", Int 1))
       Liteeffects.Ast.TInt env)

let test_interpret () =
  Alcotest.(check int)
    "interprets int" 1
    (Liteeffects.Interpret.interpret_start (Int 1));
  Alcotest.(check int)
    "interprets add" 3
    (Liteeffects.Interpret.interpret_start (Add (Int 1, Int 2)));
  Alcotest.(check int)
    "interprets mult" 6
    (Liteeffects.Interpret.interpret_start (Mult (Int 3, Int 2)));
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
        (Liteeffects.Interpret.interpret_start
           (Add (Lambda ([], None, Int 1), Int 2))));
  Alcotest.(check int)
    "binding use in addition interprets" 3
    (Liteeffects.Interpret.interpret_start
       (Bound ("a", None, Int 1, Add (Ref "a", Int 2))));
  Alcotest.(check int)
    "lambda binding interprets" 1
    (Liteeffects.Interpret.interpret_start
       (Bound
          ( "f",
            None,
            Lambda ([ ("a", TInt) ], None, Ref "a"),
            App ("f", [ Int 1 ]) )));
  Alcotest.(check int)
    "application of lambda interprets" 2
    (Liteeffects.Interpret.interpret_start
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
    (Liteeffects.Interpret.interpret_start
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
