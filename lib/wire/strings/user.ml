open Domain
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
    : string User.dict)

let rec decode (default : User.user) = function
  | (name, v) :: tail when name = field.id -> (
      match int_of_string_opt v with
      | Some v -> decode { default with id = UserID v } tail
      | _ -> Error (BadValue v))
  | (name, v) :: tail when name = field.first_name ->
      decode { default with first_name = v } tail
  | (name, v) :: tail when name = field.last_name ->
      decode { default with last_name = v } tail
  | (name, v) :: tail when name = field.email -> (
      match Contact.parse_email v with
      | Ok v -> decode { default with email = Some v } tail
      | _ -> Error (BadValue v))
  | (name, v) :: tail when name = field.mobile -> (
      match Contact.parse_mobile v with
      | Ok v -> decode { default with mobile = Some v } tail
      | _ -> Error (BadValue v))
  | (name, v) :: tail when name = field.address_id -> (
      match int_of_string_opt v with
      | Some v -> decode { default with address_id = Some (AddressID v) } tail
      | _ -> Error (BadValue v))
  | kv :: tail -> Error (BadField kv)
  | [] -> Ok default

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
