open Ast
module StringMap = Map.Make (String)

type scope = Ast.exp StringMap.t

(* Interprets using eager evaluation *)
let rec interpret (ast : Ast.exp) (scope : scope) : Int.t =
  match ast with
  | Int value -> value
  | Add (left, right) -> interpret left scope + interpret right scope
  | Mult (left, right) -> interpret left scope * interpret right scope
  | Ref name -> interpret (StringMap.find name scope) scope
  | App (name, args) -> (
      let func = StringMap.find name scope in
      match func with
      | Lambda (params, _return_type, body) ->
          let bindings =
            List.map2
              (fun (name, _typ) value ->
                ( name,
                  match value with
                  (* Don't interpret lambda immediately *)
                  | Lambda (_, _, _) -> value
                  (* Refs original value added with a new name *)
                  | Ref ref_name -> StringMap.find ref_name scope
                  (* Everything else evaluates *)
                  | _ -> Int (interpret value scope) ))
              params args
          in
          let lambda_scope = StringMap.add_seq (List.to_seq bindings) scope in
          interpret body lambda_scope
      | _ -> failwith (name ^ " is not a function"))
  | Bound (name, _typ, exp, next) -> (
      match exp with
      | Lambda (_, _, _) -> interpret next StringMap.(scope |> add name exp)
      | _ ->
          let value = interpret exp scope in
          interpret next StringMap.(scope |> add name (Int value)))
  | _ -> failwith "Not implemented or the ast hasn't been typechecked"

let interpret_start ast = interpret ast StringMap.empty
