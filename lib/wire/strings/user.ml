open Domain
open User
open Common
open Error

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

let decode data =
  let open Fields in
  let id, data = one data field.id in
  let first_name, data = one data field.first_name in
  let last_name, data = one data field.last_name in
  let email, data = one_opt data field.email in
  let mobile, data = one_opt data field.mobile in
  let address_id, data = one_opt data field.address_id in

  let none_or_valid f v =
    Result.bind v
    @@ Option.fold ~none:(Ok None) ~some:(fun v ->
        f v |> Result.map_error (fun _ -> BadValue v) |> Result.map Option.some)
    |> map_err
  in

  (let+ _ =
     match data with
     | [] -> Ok ()
     | _ -> Error (List.map (fun v -> BadField v) data)
   and+ id =
     Result.bind id (fun v ->
         Option.map (fun v -> UserID v) @@ int_of_string_opt v
         |> Option.to_result ~none:(BadValue v))
     |> map_err
   and+ first_name = first_name |> map_err
   and+ last_name = last_name |> map_err
   and+ email = none_or_valid Contact.parse_email email
   and+ mobile = none_or_valid Contact.parse_mobile mobile
   and+ address_id =
     none_or_valid
       (fun v ->
         int_of_string_opt v
         |> Option.map (fun v -> Location.AddressID v)
         |> Option.to_result ~none:())
       address_id
   in
   { id; first_name; last_name; email; mobile; address_id })
  |> Result.map_error Error.many

let encode
    ({ id; first_name; last_name; email; mobile; address_id } : User.user) =
  User.zip field
    ({
       id = string_of_int @@ User.id_to_int id;
       first_name;
       last_name;
       email = Option.fold ~none:"" ~some:Contact.email_to_string email;
       mobile = Option.fold ~none:"" ~some:Contact.mobile_to_string mobile;
       address_id =
         Option.fold ~none:""
           ~some:(fun v -> string_of_int @@ Location.id_to_int v)
           address_id;
     }
      : string User.dict)
