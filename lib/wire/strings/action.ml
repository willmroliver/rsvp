open Core.Common.Action
open Error

let field = "action"

let kind_decode = function
  | "create" -> Ok Create
  | "read" -> Ok Read
  | "update" -> Ok Update
  | "delete" -> Ok Delete
  | v -> Error (BadValue v)

let kind_encode = function
  | Create -> "create"
  | Read -> "read"
  | Update -> "update"
  | Delete -> "delete"

let encode k = [ (field, kind_encode k) ]

let decode ?kind = function
  | [ (f, k) ] when f = field ->
      Option.fold ~none:(kind_decode k)
        ~some:(fun v -> if kind_encode v = k then Ok v else Error (BadValue k))
        kind
  | kv :: l -> Error (BadField kv)
  | [] -> Error (MissingRequired field)

let partition ?kind (fields : (string * string) list) =
  let a, fields = List.partition (fun (f, _) -> f = field) fields in
  match kind with
  | Some kind -> (decode ~kind a, fields)
  | None -> (decode a, fields)
