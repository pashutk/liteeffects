open Ast
module StringMap = Map.Make (String)

type scope = Ast.exp StringMap.t

(* Interprets using eager evaluation *)
let rec interpret (ast : Ast.exp) (scope : scope) : Int.t =
  match ast with
  | Int value -> value
  | Add (Int left, Int right) -> left + right
  | Add (left, right) ->
      let exp = Add (Int (interpret left scope), Int (interpret right scope)) in
      interpret exp scope
  | Mult (Int left, Int right) -> left * right
  | Mult (left, right) ->
      let exp =
        Mult (Int (interpret left scope), Int (interpret right scope))
      in
      interpret exp scope
  | (Ref name | App (name, _)) when not (StringMap.mem name scope) ->
      failwith (name ^ " is not defined")
  | Ref name -> interpret (StringMap.find name scope) scope
  | App (name, args) -> (
      let interpreted_args =
        List.(args |> map (fun arg -> Int (interpret arg scope)))
      in
      let lambda = StringMap.find name scope in
      match lambda with
      | Lambda (params, _return_type, body) ->
          let bindings =
            List.map2
              (fun (name, _typ) value -> (name, value))
              params interpreted_args
          in
          let lambda_scope = StringMap.add_seq (List.to_seq bindings) scope in
          interpret body lambda_scope
      | _ -> failwith (name ^ " is not a function"))
  | Bound (name, _typ, exp, next) ->
      let value = interpret exp scope in
      interpret next StringMap.(scope |> add name (Int value))
  | _ -> failwith "Not implemented or the ast hasn't been typechecked"

let interpret_start ast = interpret ast StringMap.empty
