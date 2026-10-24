open Domain
open Wire.Strings.Rsvp

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
    : string Rsvp.dict)

let fields = Rsvp.zip labels fields

let status_to_display = function
  | Rsvp.Coming -> "Coming!"
  | Rsvp.MaybeComing -> "Might make it"
  | Rsvp.NotComing -> "Won't be there :("

let status_opts =
  Rsvp.fold_status
    (fun v acc -> (status_to_display v, status_encode v) :: acc)
    []

let transport_to_display = function
  | Requirement.NoTransport -> "None"
  | Requirement.Arriving -> "Arriving"
  | Requirement.Leaving -> "Leaving"
  | Requirement.BothWays -> "Both"

let transport_opts =
  Requirement.fold_transport
    (fun v acc -> (transport_to_display v, transport_encode v) :: acc)
    []

let dietary_to_display = function
  | Requirement.NoDietary -> "None"
  | Requirement.Vegetarian -> "Vegetarian"
  | Requirement.Vegan -> "Vegan"
  | Requirement.Pescatarian -> "Pescatarian"
  | Requirement.Allergies _ -> "Allergies"

let dietary_opts =
  Requirement.fold_dietary
    (fun v acc -> (dietary_to_display v, dietary_encode v) :: acc)
    []

let rsvp_to_display (rsvp : Rsvp.rsvp) =
  ({
     user_id = string_of_int @@ Rsvp.id_to_int rsvp.user_id;
     status = status_to_display rsvp.status;
     arriving_from = city_encode rsvp.arriving_from;
     returning_to = city_encode rsvp.returning_to;
     transport = transport_to_display rsvp.transport;
     dietary = dietary_to_display rsvp.dietary;
     plus_one = Option.fold ~none:"" ~some:Proof.Strings.to_string rsvp.plus_one;
   }
    : string Rsvp.dict)
