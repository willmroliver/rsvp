open Core.Rsvp
open Domain
open Common.Error

let status_decode = function
  | "coming" -> Ok Rsvp.Coming
  | "maybe_coming" -> Ok Rsvp.MaybeComing
  | "not_coming" -> Ok Rsvp.NotComing
  | v -> Error (BadValue v)

let status_encode = function
  | Rsvp.Coming -> "coming"
  | Rsvp.MaybeComing -> "maybe_coming"
  | Rsvp.NotComing -> "not_coming"

let city_decode s =
  Proof.Strings.non_empty @@ Proof.Strings.of_string s
  |> Option.fold ~none:(Error (BadValue "")) ~some:(fun c ->
      Ok (Location.City c))

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
          if key <> "allergies" then Error (BadValue key)
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
    plus_one = "plus_one";
  }

open Domain.Rsvp
open Common
open Fields

let decode data =
  let status, data = one data fields.status in
  let arriving_from, data = one_opt data fields.arriving_from in
  let returning_to, data = one_opt data fields.returning_to in
  let transport, data = one data fields.transport in
  let dietary, data = one data fields.dietary in
  let plus_one, data = one_opt data fields.plus_one in

  (let+ _ =
     match data with
     | [] -> Ok ()
     | data -> Error (List.map (fun v -> BadField v) data)
   and+ status = Result.bind status status_decode |> map_err
   and+ arriving_from =
     Result.bind arriving_from (fun v ->
         v
         |> Option.fold ~none:(Ok None) ~some:(fun v ->
             city_decode v |> Result.map Option.some))
     |> map_err
   and+ returning_to =
     Result.bind returning_to (fun v ->
         v
         |> Option.fold ~none:(Ok None) ~some:(fun v ->
             city_decode v |> Result.map Option.some))
     |> map_err
   and+ transport = Result.bind transport transport_decode |> map_err
   and+ dietary = Result.bind dietary dietary_decode |> map_err
   and+ plus_one =
     Result.bind plus_one (fun v ->
         v
         |> Option.fold ~none:(Ok None) ~some:(fun v ->
             Proof.Strings.of_string v |> Proof.Strings.non_empty
             |> Option.to_result ~none:(BadValue v)
             |> Result.map Option.some))
     |> map_err
   in
   { status; arriving_from; returning_to; transport; dietary; plus_one })
  |> Result.map_error many
