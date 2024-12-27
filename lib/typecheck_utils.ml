module StringMap = Map.Make (String)

type env_t = Ast.typ StringMap.t
