open Core.Rsvp
open Domain
open Error

let status_decode = function
  | "" | "not_responded" -> Ok Rsvp.NotResponded
  | "coming" -> Ok Rsvp.Coming
  | "maybe_coming" -> Ok Rsvp.MaybeComing
  | "not_coming" -> Ok Rsvp.NotComing
  | v -> Error (BadValue v)

let status_encode = function
  | Rsvp.NotResponded -> "not_responded"
  | Rsvp.Coming -> "coming"
  | Rsvp.MaybeComing -> "maybe_coming"
  | Rsvp.NotComing -> "not_coming"

let city_decode s =
  Proof.Strings.non_empty @@ Proof.Strings.of_string s
  |> Option.fold ~none:(Error (BadValue "")) ~some:(fun c -> Ok c)

let transport_decode = function
  | "" | "none" -> Ok Requirement.NoTransport
  | "arriving" -> Ok Requirement.Arriving
  | "leaving" -> Ok Requirement.Leaving
  | "both" -> Ok Requirement.BothWays
  | s -> Error (BadValue s)

let transport_encode = function
  | Requirement.NoTransport -> "none"
  | Requirement.Arriving -> "arriving"
  | Requirement.Leaving -> "leaving"
  | Requirement.BothWays -> "both"

let dietary_decode = function
  | "" | "none" -> Ok Requirement.NoDietary
  | "vegetarian" -> Ok Requirement.Vegetarian
  | "pescatarian" -> Ok Requirement.Pescatarian
  | "vegan" -> Ok Requirement.Vegan
  | s -> (
      match String.split_on_char ',' s with
      | key :: l ->
          if key != "allergies" then Error (BadValue key)
          else
            List.map
              (fun a ->
                match Proof.Strings.alpha @@ Proof.Strings.of_string a with
                | Some a -> Ok a
                | None -> Error a)
              l
            |> List.fold_left
                 (fun acc opt ->
                   match (acc, opt) with
                   | Ok (Requirement.Allergies l), Ok s ->
                       Ok (Requirement.Allergies (s :: l))
                   | _, Error s -> Error (BadValue s)
                   | (Error _ as e), _ -> e
                   | _ -> Error Unknown)
                 (Ok (Requirement.Allergies []))
      | [] -> Error (BadValue s))

let dietary_encode = function
  | Requirement.NoDietary -> "none"
  | Requirement.Vegetarian -> "vegetarian"
  | Requirement.Pescatarian -> "pescatarian"
  | Requirement.Vegan -> "vegan"
  | Requirement.Allergies l ->
      "allergies," ^ String.concat "," (List.map Proof.Strings.to_string l)

let fields : string Rsvp.dict =
  {
    status = "status";
    arriving_from = "arriving_from";
    returning_to = "returning_to";
    transport = "transport";
    dietary = "dietary";
    plus_ones = "plus_ones";
  }

open Domain.Rsvp

let rec rsvp_decode default = function
  | (name, v) :: tail when name = fields.status -> (
      match status_decode v with
      | Ok status -> rsvp_decode { default with status } tail
      | Error _ -> Error (BadField (name, v)))
  | (name, v) :: tail when name = fields.arriving_from -> (
      match city_decode v with
      | Ok city ->
          rsvp_decode
            { default with arriving_from = Some (Location.City city) }
            tail
      | Error (BadValue "") ->
          rsvp_decode { default with arriving_from = None } tail
      | Error _ -> Error (BadField (name, v)))
  | (name, v) :: tail when name = fields.returning_to -> (
      match city_decode v with
      | Ok city ->
          rsvp_decode
            { default with returning_to = Some (Location.City city) }
            tail
      | Error (BadValue "") ->
          rsvp_decode { default with arriving_from = None } tail
      | Error _ -> Error (BadField (name, v)))
  | (name, v) :: tail when name = fields.transport -> (
      match transport_decode v with
      | Ok transport -> rsvp_decode { default with transport } tail
      | Error _ -> Error (BadField (name, v)))
  | (name, v) :: tail when name = fields.dietary -> (
      match dietary_decode v with
      | Ok dietary -> rsvp_decode { default with dietary } tail
      | Error _ -> Error (BadField (name, v)))
  | (name, v) :: tail when name = fields.plus_ones -> (
      match int_of_string_opt v with
      | Some plus_ones -> rsvp_decode { default with plus_ones } tail
      | None -> Error (BadField (name, v)))
  | (name, v) :: fields -> Error (BadField (name, v))
  | [] -> Ok default
