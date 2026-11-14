open Domain.Location
open Common
open Error

let id_codec = Tool.Codec.map id_to_int (fun v -> AddressID v) Basic.int_codec

let city_codec =
  Tool.Codec.
    {
      encode = (fun v -> match v with City v -> v |> Proof.Strings.to_string);
      decode =
        (fun v ->
          Proof.Strings.non_empty @@ Proof.Strings.of_string v
          |> Option.fold ~none:(Error (BadValue v)) ~some:(fun c -> Ok (City c)));
    }
