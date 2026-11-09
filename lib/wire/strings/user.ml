open Domain.User
open Common

let field =
  ({
     id = "id";
     first_name = "first_name";
     last_name = "last_name";
     email = "email";
     mobile = "mobile";
     address_id = "address_id";
   }
    : string dict)

let id_decode v = Basic.int_decode v |> Result.map (fun v -> UserID v)
let id_encode v = string_of_int @@ id_to_int v

let decode data =
  let open Fields in
  let id, data = one data field.id in
  let first_name, data = one data field.first_name in
  let last_name, data = one data field.last_name in
  let email, data = one_opt data field.email in
  let mobile, data = one_opt data field.mobile in
  let address_id, data = one_opt data field.address_id in

  Decode.(
    let+ _ =
      match data with
      | [] -> Ok ()
      | _ -> Error (List.map (fun v -> Error.BadField v) data |> Error.many)
    and+ id = Decode.field_result field.id id id_decode
    and+ first_name = first_name
    and+ last_name = last_name
    and+ email =
      Decode.field_result field.email email
      @@ Basic.option_decode Contact.email_decode
    and+ mobile =
      Decode.field_result field.mobile mobile
      @@ Basic.option_decode @@ Contact.mobile_decode
    and+ address_id =
      Decode.field_result field.address_id address_id
      @@ Basic.option_decode Location.address_id_decode
    in
    { id; first_name; last_name; email; mobile; address_id })

let encode ({ id; first_name; last_name; email; mobile; address_id } : user) =
  zip field
    ({
       id = id_encode id;
       first_name;
       last_name;
       email = Contact.email_encode email;
       mobile = Contact.mobile_encode mobile;
       address_id = Location.address_id_encode address_id;
     }
      : string dict)
