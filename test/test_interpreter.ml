open Liteeffects.Interpret
open Liteeffects.Ast

let test_int () =
  Alcotest.(check int) "interprets int" 1 (interpret_empty (Int 1))

let test_add () =
  Alcotest.(check int) "interprets add" 3 (interpret_empty (Add (Int 1, Int 2)));
  Alcotest.(check int)
    "addition of ref and int interprets" 5
    (interpret
       (Add (Ref "a", Int 2))
       (let a = Int 3 in
        StringMap.(empty |> add "a" a)));
  Alcotest.(check int)
    "addition of application and int interprets" 6
    (interpret
       (* f(1) + 2 *)
       (Add (App ("f", [ Int 1 ]), Int 2))
       (let f =
          (* a => a + 3 *)
          Lambda ([ ("a", TInt) ], None, None, Add (Ref "a", Int 3))
        in
        StringMap.(empty |> add "f" f)));
  Alcotest.check_raises "interpreting addition of anything else fails"
    (Failure "Not implemented or the ast hasn't been typechecked") (fun () ->
      ignore (interpret_empty (Add (Lambda ([], None, None, Int 1), Int 2))))

let test_mult () =
  Alcotest.(check int)
    "interprets mult" 6
    (interpret_empty (Mult (Int 3, Int 2)))

let test_bound () =
  Alcotest.(check int)
    "binding use in addition interprets" 3
    (interpret_empty (Bound ("a", None, Int 1, Add (Ref "a", Int 2))));
  Alcotest.(check int)
    "lambda binding interprets" 1
    (interpret_empty
       (Bound
          ( "f",
            None,
            Lambda ([ ("a", TInt) ], None, None, Ref "a"),
            App ("f", [ Int 1 ]) )));
  Alcotest.(check int)
    "application of lambda interprets" 2
    (interpret_empty
       (* const app = (f, value) => f(value); app((a) => a + 1, 1) *)
       ((* (Int) => Int *)
        let f_param_type = TLambda ([ TInt ], None, TInt) in
        (* (f: f_param_type, value: Int) => f(value) *)
        let app =
          Lambda
            ( [ ("f", f_param_type); ("value", TInt) ],
              None,
              None,
              App ("f", [ Ref "value" ]) )
        in
        let inc = Lambda ([ ("a", TInt) ], None, None, Add (Ref "a", Int 1)) in
        let result = App ("app", [ inc; Int 1 ]) in
        Bound ("app", None, app, result)));
  Alcotest.(check int)
    "application of bound lambda interprets" 2
    (interpret_empty
       (* const inc = (a) => a + 1; const app = (f, value) => f(value); app(inc, 1) *)
       (let inc = Lambda ([ ("a", TInt) ], None, None, Add (Ref "a", Int 1)) in
        let f_param_type = TLambda ([ TInt ], None, TInt) in
        let app =
          Lambda
            ( [ ("f", f_param_type); ("value", TInt) ],
              None,
              None,
              App ("f", [ Ref "value" ]) )
        in
        let result = App ("app", [ Ref "inc"; Int 1 ]) in

        Bound ("inc", None, inc, Bound ("app", None, app, result))))

let () =
  let open Alcotest in
  run "Interpreter"
    [
      ("Add", [ test_case "Add" `Quick test_add ]);
      ("Int", [ test_case "Int" `Quick test_int ]);
      ("Mult", [ test_case "Mult" `Quick test_mult ]);
      ("Bound", [ test_case "Bound" `Quick test_bound ]);
    ]
