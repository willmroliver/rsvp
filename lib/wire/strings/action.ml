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
