open Common
open Codec
open Error

let maybe_encode f o = Option.fold ~none:"" ~some:f o

let maybe_decode f v =
  Option.fold ~none:(Ok None) ~some:(fun v -> f v |> Result.map Option.some) v
