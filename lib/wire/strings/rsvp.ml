open Core.Rsvp
open Domain.Rsvp
open Common
open Error
open Fields

let fields : string dict =
  {
    user_id = "user_id";
    status = "status";
    arriving_from = "arriving_from";
    returning_to = "returning_to";
    transport = "transport";
    dietary = "dietary";
    plus_one = "plus_one";
  }

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

let decode data =
  let user_id, data = one data fields.user_id in
  let status, data = one data fields.status in
  let arriving_from, data = one data fields.arriving_from in
  let returning_to, data = one data fields.returning_to in
  let transport, data = one data fields.transport in
  let dietary, data = one data fields.dietary in
  let plus_one, data = one data fields.plus_one in

  let open Decode in
  let+ _ =
    match data with
    | [] -> Ok ()
    | data -> Error (List.map (fun v -> BadField v) data |> many)
  and+ user_id = Decode.field_result fields.user_id user_id User.id_codec.decode
  and+ status = Decode.field_result fields.status status status_codec.decode
  and+ arriving_from =
    Decode.field_result fields.arriving_from arriving_from city_codec.decode
  and+ returning_to =
    Decode.field_result fields.returning_to returning_to city_codec.decode
  and+ transport =
    Decode.field_result fields.transport transport
      Requirement.transport_codec.decode
  and+ dietary =
    Decode.field_result fields.dietary dietary Requirement.dietary_codec.decode
  and+ plus_one =
    Decode.field_result fields.plus_one plus_one plus_one_codec.decode
  in
  { user_id; status; arriving_from; returning_to; transport; dietary; plus_one }

let encode
    ({
       user_id;
       status;
       arriving_from;
       returning_to;
       transport;
       dietary;
       plus_one;
     } :
      rsvp) =
  zip fields
    {
      user_id = string_of_int @@ id_to_int user_id;
      status = status_codec.encode status;
      arriving_from = city_codec.encode arriving_from;
      returning_to = city_codec.encode returning_to;
      transport = Requirement.transport_codec.encode transport;
      dietary = Requirement.dietary_codec.encode dietary;
      plus_one = plus_one_codec.encode plus_one;
    }
