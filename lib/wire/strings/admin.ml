open Common
open Domain
open Error
open Tool.Codec

module Users = struct
  let field_id = "id"

  let get_codec =
    {
      encode = (fun id -> [ (field_id, string_of_int @@ User.id_to_int id) ]);
      decode =
        (fun data ->
          match data with
          | (name, id) :: _ when name = field_id ->
              int_of_string_opt id
              |> Option.map (fun id -> User.UserID id)
              |> Option.to_result ~none:(BadField (name, id))
          | kv :: tail -> Error (BadField kv)
          | [] -> Error (MissingRequired field_id));
    }

  let delete_codec = get_codec
end
