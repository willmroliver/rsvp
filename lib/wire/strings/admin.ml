open Domain
open Common
open Error

module Users = struct
  let field_id = "id"

  (* get *)
  let get_encode id = [ (field_id, string_of_int @@ User.id_to_int id) ]

  let get_decode data =
    match data with
    | (name, id) :: _ when name = field_id ->
        int_of_string_opt id
        |> Option.map (fun id -> User.UserID id)
        |> Option.to_result ~none:(BadValue (name, id))
    | kv :: tail -> Error (BadField kv)
    | [] -> Error (MissingRequired field_id)

  (* delete *)
  let delete_encode = get_encode
  let delete_decode = get_decode
end
