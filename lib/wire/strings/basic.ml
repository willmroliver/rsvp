open Common
open Error

let int_decode v = int_of_string_opt v |> Option.to_result ~none:(BadValue v)
let option_encode f o = Option.fold ~none:"" ~some:f o

let option_decode f v =
  Option.fold ~none:(Ok None) ~some:(fun v -> f v |> Result.map Option.some) v
