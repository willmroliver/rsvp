open Core.Common
open Error

let rec list_args_decode default = function
  | ("from", n) :: tail -> (
      match int_of_string_opt n with
      | Some n -> list_args_decode { default with off = n } tail
      | None -> Error (BadValue n))
  | ("size", n) :: tail -> (
      match int_of_string_opt n with
      | Some n -> list_args_decode { default with lim = n } tail
      | None -> Error (BadValue n))
  | [] -> Ok default
  | kv :: tail -> Error (BadField kv)
