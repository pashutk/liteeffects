module StringMap = Map.Make (String)

type effect = Ast.typ StringMap.t
type t = { effects : effect StringMap.t; bindings : Ast.typ StringMap.t }

let empty = { effects = StringMap.empty; bindings = StringMap.empty }

let add_binding name binding env =
  {
    bindings = env.bindings |> StringMap.add name binding;
    effects = env.effects;
  }

let find_binding_opt name env = StringMap.find_opt name env.bindings
let has_binding name env = StringMap.mem name env.bindings

let add_effect name effect env =
  {
    bindings = env.bindings;
    effects = env.effects |> StringMap.add name effect;
  }

let find_effect_opt name env = StringMap.find_opt name env.effects
let has_effect name env = StringMap.mem name env.effects
