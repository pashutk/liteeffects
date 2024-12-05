let ltts = function
  | Ast.Int _i -> "int"
  | Ast.Add (_a, _b) -> "add"
  | Ast.Const (id, _exp) -> "const " ^ id

let main () =
  (* let lexbuf = Lexing.from_channel stdin in *)
  let lexbuf = Lexing.from_string "const x = 1" in
  let b = Parser.main Lexer.token lexbuf in

  print_endline (ltts b)
