let main () =
  (* let lexbuf = Lexing.from_channel stdin in *)
  let lexbuf =
    Lexing.from_string
      "const addAndLet = () => {\n\
       const x = 5;\n\
       const y = 3;\n\
       const result = x + y * 2;\n\
       result\n\
       };\n\
       addAndLet()"
  in
  let ast = lexbuf |> Parser.main Lexer.token in
  let typecheck_result = Typecheck.check_main ast in
  match typecheck_result with
  | Ok _ -> ast |> Interpret.interpret_empty |> Int.to_string |> print_endline
  | Error err -> Typecheck_utils.pp_type_error Format.std_formatter err
