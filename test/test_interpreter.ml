open Alcotest

let test_int () =
  Alcotest.(check int)
    "interprets int" 1
    (Liteeffects.Interpret.interpret_empty (Int 1))

let test_add () =
  Alcotest.(check int)
    "interprets add" 3
    (Liteeffects.Interpret.interpret_empty (Add (Int 1, Int 2)));
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
           (Add (Lambda ([], None, Int 1), Int 2))))

let test_mult () =
  Alcotest.(check int)
    "interprets mult" 6
    (Liteeffects.Interpret.interpret_empty (Mult (Int 3, Int 2)))

let test_bound () =
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

let test_suite =
  [
    test_case "Add" `Quick test_add;
    test_case "Int" `Quick test_int;
    test_case "Mult" `Quick test_mult;
    test_case "Bound" `Quick test_bound;
  ]
