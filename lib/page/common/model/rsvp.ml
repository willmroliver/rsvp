open Domain.Rsvp
open Domain.Requirement
open Wire.Strings

let labels : type a. a field -> string = function
  | UserID -> "User"
  | Status -> "Are you free?"
  | ArrivingFrom -> "Arriving from"
  | ReturningTo -> "Returning to"
  | Transport -> "Transport help needed?"
  | Dietary -> "Dietary requirements"
  | PlusOne -> "Plus one?"

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

let rsvp_to_display : type a. rsvp -> a field -> string =
 fun rsvp ->
  let open Wire.Strings in
  function
  | UserID -> string_of_int @@ id_to_int rsvp.user_id
  | Status -> status_to_display rsvp.status
  | ArrivingFrom -> Rsvp.city_codec.encode rsvp.arriving_from
  | ReturningTo -> Rsvp.city_codec.encode rsvp.returning_to
  | Transport -> transport_to_display rsvp.transport
  | Dietary -> dietary_to_display rsvp.dietary
  | PlusOne -> Rsvp.plus_one_codec.encode rsvp.plus_one
