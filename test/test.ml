let () =
  Alcotest.run "Liteeffects"
    [
      ("Parser", Test_parser.test_suite);
      ("Typechecker", Test_typechecker.test_suite);
      ("Interpreter", Test_interpreter.test_suite);
    ]
