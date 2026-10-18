open Core.Common
open Error

let field = ({ off = "from"; lim = "size" } : string Listing.args_dict)

let rec args_decode (default : Listing.args) = function
  | (name, n) :: tail when name == field.off -> (
      match int_of_string_opt n with
      | Some n -> args_decode { default with off = n } tail
      | None -> Error (BadValue n))
  | (name, n) :: tail when name == field.lim -> (
      match int_of_string_opt n with
      | Some n -> args_decode { default with lim = n } tail
      | None -> Error (BadValue n))
  | [] -> Ok default
  | kv :: tail -> Error (BadField kv)
