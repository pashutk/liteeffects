open Liteeffects.Cli

let rec pp_ast fmt = function
  | Liteeffects.Ast.Int i -> Format.fprintf fmt "Int %d" i
  | Liteeffects.Ast.Add (a, b) ->
      Format.fprintf fmt "Add (%a, %a)" pp_ast a pp_ast b
  | Liteeffects.Ast.Function (name, args, exp) ->
      Format.fprintf fmt "Function (%s, %a, %a)" name
        (Format.pp_print_list Format.pp_print_string)
        args pp_ast exp
  | _ -> Format.fprintf fmt "Unknown"

let ast_testable = Alcotest.testable pp_ast ( = )

let test_int () =
  Alcotest.check ast_testable "parse '1'" (Int 1) (parse "1");
  Alcotest.check ast_testable "parse '1 + 2'"
    (Add (Int 1, Int 2))
    (parse "1 + 2")

let test_add () =
  Alcotest.check ast_testable "parse '1 + 2'"
    (Add (Int 1, Int 2))
    (parse "1 + 2")

let test_function () =
  Alcotest.check ast_testable "parse function w no args"
    (Function ("name", [], Int 1))
    (parse "function name() { 1 }");
  Alcotest.check ast_testable "parse function w one arg"
    (Function ("name", [ "a" ], Int 1))
    (parse "function name(a) { 1 }");
  Alcotest.check ast_testable "parse function w multiple args"
    (Function ("name", [ "a"; "b"; "c" ], Int 1))
    (parse "function name(a, b, c) { 1 }")

let () =
  let open Alcotest in
  run "Parser"
    [
      ( "ast",
        [
          test_case "Int" `Quick test_int;
          test_case "Add" `Quick test_add;
          test_case "Function" `Quick test_function;
        ] );
    ]
