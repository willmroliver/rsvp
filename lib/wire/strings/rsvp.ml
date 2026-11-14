open Common
open Core.Rsvp
open Error
open Fields
open Domain.Rsvp

let status_codec =
  Tool.Codec.
    {
      encode =
        (function
        | Coming -> "coming"
        | MaybeComing -> "maybe_coming"
        | NotComing -> "not_coming");
      decode =
        (function
        | "coming" -> Ok Coming
        | "maybe_coming" -> Ok MaybeComing
        | "not_coming" -> Ok NotComing
        | v -> Error (BadValue v));
    }

let city_codec = Tool.Codec.maybe ~none:"" Location.city_codec

let plus_one_codec =
  Tool.Codec.
    {
      encode = Proof.Strings.to_string;
      decode =
        (fun v ->
          Proof.Strings.of_string v |> Proof.Strings.non_empty
          |> Option.to_result ~none:(BadValue v));
    }
  |> Tool.Codec.maybe ~none:""

module Model =
  Basic.GADT.Make
    (struct
      include Domain.Rsvp

      type t = rsvp
    end)
    (struct
      type t = rsvp
      type 'a field = 'a Domain.Rsvp.field

      let get = get

      let field_name : type a. a field -> string = function
        | UserID -> "user_id"
        | Status -> "status"
        | ArrivingFrom -> "arriving_from"
        | ReturningTo -> "returning_to"
        | Transport -> "transport"
        | Dietary -> "dietary"
        | PlusOne -> "plus_one"

      let get_field_codec : type a. a field -> (a, string, Error.t) Tool.Codec.t
          = function
        | UserID -> User.id_codec
        | Status -> status_codec
        | ArrivingFrom -> city_codec
        | ReturningTo -> city_codec
        | Transport -> Requirement.transport_codec
        | Dietary -> Requirement.dietary_codec
        | PlusOne -> plus_one_codec
    end)

let codec =
  Tool.Codec.
    {
      encode = Model.encode;
      decode =
        (fun data ->
          let open Decode in
          let+ _ =
            match data with
            | [] -> Ok ()
            | _ ->
                Error (List.map (fun v -> Error.BadField v) data |> Error.many)
          and+ user_id = Model.decode data UserID
          and+ status = Model.decode data Status
          and+ arriving_from = Model.decode data ArrivingFrom
          and+ returning_to = Model.decode data ReturningTo
          and+ transport = Model.decode data Transport
          and+ dietary = Model.decode data Dietary
          and+ plus_one = Model.decode data PlusOne in
          {
            user_id;
            status;
            arriving_from;
            returning_to;
            transport;
            dietary;
            plus_one;
          });
    }
