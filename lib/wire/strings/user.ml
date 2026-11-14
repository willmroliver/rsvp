open Domain.User
open Common
open Tool

let id_codec = Codec.map id_to_int (fun v -> UserID v) Basic.int_codec
let email_codec = Codec.maybe ~none:"" Contact.email_codec
let mobile_codec = Codec.maybe ~none:"" Contact.mobile_codec
let address_codec = Codec.maybe ~none:"" Location.id_codec

let field_name : type a. a field -> string = function
  | ID -> "id"
  | FirstName -> "first_name"
  | LastName -> "last_name"
  | Email -> "email"
  | Mobile -> "mobile"
  | AddressID -> "address_id"

let field_codec : type a. a field -> (a, string, Error.t) Codec.t = function
  | ID -> id_codec
  | FirstName -> Codec.identity
  | LastName -> Codec.identity
  | Email -> email_codec
  | Mobile -> mobile_codec
  | AddressID -> address_codec

let field_encode user f = (field_codec f).encode @@ get user f

let field_decode : type a.
    (string * string) list -> a field -> (a, Error.t) result =
 fun data f ->
  let name = field_name f in
  List.filter_map (fun (k, v) -> if k = name then Some v else None) data
  |> function
  | el :: [] -> (field_codec f).decode el
  | [] -> Error (MissingRequired name)
  | _ :: l -> Error (DuplicateField name)

let codec =
  Codec.
    {
      encode =
        (fun user ->
          List.map (function Any f -> (field_name f, field_encode user f)) all);
      decode =
        (fun data ->
          Decode.(
            let+ _ =
              match data with
              | [] -> Ok ()
              | _ ->
                  Error (List.map (fun v -> Error.BadField v) data |> Error.many)
            and+ id = field_decode data ID
            and+ first_name = field_decode data FirstName
            and+ last_name = field_decode data LastName
            and+ email = field_decode data Email
            and+ mobile = field_decode data Mobile
            and+ address_id = field_decode data AddressID in
            { id; first_name; last_name; email; mobile; address_id }));
    }
