open Domain.User
open Common
open Tool

let id_codec = Codec.map id_to_int (fun v -> UserID v) Basic.int_codec
let email_codec = Codec.maybe ~none:"" Contact.email_codec
let mobile_codec = Codec.maybe ~none:"" Contact.mobile_codec
let address_codec = Codec.maybe ~none:"" Location.id_codec

module Model =
  Basic.GADT.Make
    (struct
      include Domain.User

      type t = user
    end)
    (struct
      open Domain.User

      let field_name : type a. a field -> string = function
        | ID -> "id"
        | FirstName -> "first_name"
        | LastName -> "last_name"
        | Email -> "email"
        | Mobile -> "mobile"
        | AddressID -> "address_id"

      let get_field_codec : type a. a field -> (a, string, Error.t) Codec.t =
        function
        | ID -> id_codec
        | FirstName -> Codec.identity
        | LastName -> Codec.identity
        | Email -> email_codec
        | Mobile -> mobile_codec
        | AddressID -> address_codec
    end)

let codec =
  Codec.
    {
      encode = Model.encode;
      decode =
        (fun data ->
          Decode.(
            let+ _ =
              match data with
              | [] -> Ok ()
              | _ ->
                  Error (List.map (fun v -> Error.BadField v) data |> Error.many)
            and+ id = Model.decode data ID
            and+ first_name = Model.decode data FirstName
            and+ last_name = Model.decode data LastName
            and+ email = Model.decode data Email
            and+ mobile = Model.decode data Mobile
            and+ address_id = Model.decode data AddressID in
            { id; first_name; last_name; email; mobile; address_id }));
    }
