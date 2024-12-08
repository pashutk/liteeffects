open Liteeffects.Cli

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
  | Liteeffects.Ast.Bound (name, value, exp) ->
      Format.fprintf fmt "Bound (%s, %a, %a)" name pp_ast value pp_ast exp

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
    (parse "const x = 1; const y = 2; x + y")

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
        }")

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
        ] );
    ]
