open Common
open Error
open Tool.Codec

let int_codec =
  {
    encode = string_of_int;
    decode =
      (fun v -> int_of_string_opt v |> Option.to_result ~none:(BadValue v));
  }
