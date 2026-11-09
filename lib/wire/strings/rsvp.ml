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

let status_decode = function
  | "coming" -> Ok Coming
  | "maybe_coming" -> Ok MaybeComing
  | "not_coming" -> Ok NotComing
  | v -> Error (BadValue v)

let status_encode = function
  | Coming -> "coming"
  | MaybeComing -> "maybe_coming"
  | NotComing -> "not_coming"

let decode data =
  let user_id, data = one data fields.user_id in
  let status, data = one data fields.status in
  let arriving_from, data = one_opt data fields.arriving_from in
  let returning_to, data = one_opt data fields.returning_to in
  let transport, data = one data fields.transport in
  let dietary, data = one data fields.dietary in
  let plus_one, data = one_opt data fields.plus_one in

  let open Decode in
  let+ _ =
    match data with
    | [] -> Ok ()
    | data -> Error (List.map (fun v -> BadField v) data |> many)
  and+ user_id = Decode.field_result fields.user_id user_id User.id_decode
  and+ status = Decode.field_result fields.status status status_decode
  and+ arriving_from =
    Decode.field_result fields.arriving_from arriving_from
    @@ Basic.option_decode Location.city_decode
  and+ returning_to =
    Decode.field_result fields.returning_to returning_to
    @@ Basic.option_decode Location.city_decode
  and+ transport =
    Decode.field_result fields.transport transport Requirement.transport_decode
  and+ dietary =
    Decode.field_result fields.dietary dietary Requirement.dietary_decode
  and+ plus_one =
    Decode.field_result fields.plus_one plus_one
    @@ Basic.option_decode (fun v ->
        Proof.Strings.of_string v |> Proof.Strings.non_empty
        |> Option.to_result ~none:(BadValue v))
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
      status = status_encode status;
      arriving_from = Basic.option_encode Location.city_encode arriving_from;
      returning_to = Basic.option_encode Location.city_encode returning_to;
      transport = Requirement.transport_encode transport;
      dietary = Requirement.dietary_encode dietary;
      plus_one = Basic.option_encode Proof.Strings.to_string plus_one;
    }
