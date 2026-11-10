open Domain.Rsvp
open Domain.Requirement
open Wire.Strings

let labels =
  ({
     user_id = "User";
     status = "Are you free?";
     arriving_from = "Arriving from";
     returning_to = "Returning to";
     transport = "Transport help needed?";
     dietary = "Dietary requirements";
     plus_one = "Plus one?";
   }
    : string dict)

let fields = zip labels Rsvp.fields

let status_to_display = function
  | Coming -> "Coming!"
  | MaybeComing -> "Might make it"
  | NotComing -> "Won't be there :("

let status_opts =
  fold_status
    (fun v acc -> (status_to_display v, Rsvp.status_codec.encode v) :: acc)
    []

let transport_to_display = function
  | NoTransport -> "None"
  | Arriving -> "Arriving"
  | Leaving -> "Leaving"
  | BothWays -> "Both"

let transport_opts =
  fold_transport
    (fun v acc ->
      (transport_to_display v, Requirement.transport_codec.encode v) :: acc)
    []

let dietary_to_display = function
  | NoDietary -> "None"
  | Vegetarian -> "Vegetarian"
  | Vegan -> "Vegan"
  | Pescatarian -> "Pescatarian"
  | Allergies _ -> "Allergies"

let dietary_opts =
  fold_dietary
    (fun v acc ->
      (dietary_to_display v, Requirement.dietary_codec.encode v) :: acc)
    []

let rsvp_to_display (rsvp : rsvp) =
  (let open Wire.Strings in
   {
     user_id = string_of_int @@ id_to_int rsvp.user_id;
     status = status_to_display rsvp.status;
     arriving_from = Rsvp.city_codec.encode rsvp.arriving_from;
     returning_to = Rsvp.city_codec.encode rsvp.returning_to;
     transport = transport_to_display rsvp.transport;
     dietary = dietary_to_display rsvp.dietary;
     plus_one = Rsvp.plus_one_codec.encode rsvp.plus_one;
   }
    : string dict)
