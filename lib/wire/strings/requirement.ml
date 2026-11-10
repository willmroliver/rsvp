open Domain.Requirement
open Common
open Error

let transport_codec =
  Codec.
    {
      encode =
        (function
        | NoTransport -> "none"
        | Arriving -> "arriving"
        | Leaving -> "leaving"
        | BothWays -> "both");
      decode =
        (function
        | "" | "none" -> Ok NoTransport
        | "arriving" -> Ok Arriving
        | "leaving" -> Ok Leaving
        | "both" -> Ok BothWays
        | s -> Error (BadValue s));
    }

let dietary_codec =
  Codec.
    {
      encode =
        (function
        | NoDietary -> "none"
        | Vegetarian -> "vegetarian"
        | Pescatarian -> "pescatarian"
        | Vegan -> "vegan"
        | Allergies l ->
            "allergies,"
            ^ String.concat "," (List.map Proof.Strings.to_string l));
      decode =
        (function
        | "" | "none" -> Ok NoDietary
        | "vegetarian" -> Ok Vegetarian
        | "pescatarian" -> Ok Pescatarian
        | "vegan" -> Ok Vegan
        | s -> (
            match String.split_on_char ',' s with
            | key :: l ->
                if key <> "allergies" then Error (BadValue "")
                else
                  List.map
                    (fun a ->
                      match
                        Proof.Strings.alpha @@ Proof.Strings.of_string a
                      with
                      | Some a -> Ok a
                      | None -> Error a)
                    l
                  |> List.fold_left
                       (fun acc opt ->
                         match (acc, opt) with
                         | Ok (Allergies l), Ok s -> Ok (Allergies (s :: l))
                         | _, Error s -> Error (BadValue s)
                         | (Error _ as e), _ -> e
                         | _ -> Error Unknown)
                       (Ok (Allergies []))
            | [] -> Error (BadValue "")));
    }
