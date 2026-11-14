open Core.Common.Action
open Common.Error
open Tool

let field = "action"

let kind_codec =
  Codec.
    {
      encode =
        (function
        | Create -> "create"
        | Read -> "read"
        | Update -> "update"
        | Delete -> "delete");
      decode =
        (function
        | "create" -> Ok Create
        | "read" -> Ok Read
        | "update" -> Ok Update
        | "delete" -> Ok Delete
        | v -> Error (BadField (field, v)));
    }

let codec =
  Codec.product
    (Codec.const Codec.identity field (fun v -> BadField (field, v)))
    kind_codec

let partition data =
  List.partition (fun (f, _) -> f = field) data |> fun (l, data) ->
  match (l, data) with
  | [ v ], data -> (codec.decode v |> Result.map snd, data)
  | [], data -> (Error (MissingRequired field), data)
  | l, data -> (Error (DuplicateField field), data)
