open Domain
open Error

module Users = struct
  let field_id = "id"
  let get_encode id = [ (field_id, string_of_int @@ User.id_to_int id) ]

  let get_decode data =
    match data with
    | (name, id) :: _ when name = field_id ->
        int_of_string_opt id
        |> Option.map (fun id -> User.UserID id)
        |> Option.to_result ~none:(BadValue id)
    | kv :: tail -> Error (BadField kv)
    | [] -> Error (MissingRequired field_id)

  let delete_encode = get_encode
  let delete_decode = get_decode
end
