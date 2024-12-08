let rec ltts = function
  | Ast.Int _i -> "int"
  | Ast.Add (_a, _b) -> "add"
  | Ast.Bound (id, _value, _exp) -> "const " ^ id
  | Ast.Function (name, _args, exp) -> "fn " ^ name ^ ": " ^ ltts exp
  | Ast.Ref name -> "ref " ^ name
  | Ast.Mult (left, right) -> "mult " ^ ltts left ^ " * " ^ ltts right

let lbts = function
  | Parser.FUNCTION -> "fun"
  | Parser.ID name -> "id " ^ name
  | _ -> "other"

let parse s =
  let lexbuf = Lexing.from_string s in
  Parser.main Lexer.token lexbuf

let main () =
  (* let lexbuf = Lexing.from_channel stdin in *)
  let lexbuf = Lexing.from_string "function sub (a, b) { const x = 1; x }" in
  (* let lexbuf = Lexing.from_string "const x = 1; x" in *)
  (* print_endline (lbts (Lexer.token lexbuf)) *)
  let b = Parser.main Lexer.token lexbuf in

  print_endline (ltts b)
