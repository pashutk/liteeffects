let ltts = function
  | Ast.Int _i -> "int"
  | Ast.Add (_a, _b) -> "add"
  | Ast.Const (id, _exp) -> "const " ^ id
  | Ast.Function (name, _args, _exp) -> "fn " ^ name

let lbts = function Parser.FUNCTION -> "fun" | _ -> "other"

let parse s =
  let lexbuf = Lexing.from_string s in
  Parser.main Lexer.token lexbuf

let main () =
  (* let lexbuf = Lexing.from_channel stdin in *)
  let lexbuf = Lexing.from_string "function sub (a, b)" in
  (* print_endline (lbts (Lexer.token lexbuf)) *)
  let b = Parser.main Lexer.token lexbuf in

  print_endline (ltts b)
