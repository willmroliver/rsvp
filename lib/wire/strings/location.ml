open Domain.Location
open Common
open Error

let address_id_decode v =
  Basic.int_decode v |> Result.map (fun v -> AddressID v)

let address_id_encode v =
  Basic.option_encode (fun v -> string_of_int @@ Domain.Location.id_to_int v) v

let city_decode v =
  Proof.Strings.non_empty @@ Proof.Strings.of_string v
  |> Option.fold ~none:(Error (BadValue v)) ~some:(fun c -> Ok (City c))

let city_encode v = match v with City v -> v |> Proof.Strings.to_string
