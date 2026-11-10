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

let id_codec = Codec.map id_to_int (fun v -> UserID v) Codec.ints
let email_codec = Codec.maybe ~none:"" Contact.email_codec
let mobile_codec = Codec.maybe ~none:"" Contact.mobile_codec
let address_codec = Codec.maybe ~none:"" Location.id_codec

let decode data =
  let open Fields in
  let id, data = one data field.id in
  let first_name, data = one data field.first_name in
  let last_name, data = one data field.last_name in
  let email, data = one data field.email in
  let mobile, data = one data field.mobile in
  let address_id, data = one data field.address_id in

  Decode.(
    let+ _ =
      match data with
      | [] -> Ok ()
      | _ -> Error (List.map (fun v -> Error.BadField v) data |> Error.many)
    and+ id = Decode.field_result field.id id id_codec.decode
    and+ first_name = first_name
    and+ last_name = last_name
    and+ email = Decode.field_result field.email email email_codec.decode
    and+ mobile = Decode.field_result field.mobile mobile mobile_codec.decode
    and+ address_id =
      Decode.field_result field.address_id address_id address_codec.decode
    in
    { id; first_name; last_name; email; mobile; address_id })

let encode ({ id; first_name; last_name; email; mobile; address_id } : user) =
  zip field
    ({
       id = id_codec.encode id;
       first_name;
       last_name;
       email = email_codec.encode email;
       mobile = mobile_codec.encode mobile;
       address_id = address_codec.encode address_id;
     }
      : string dict)
