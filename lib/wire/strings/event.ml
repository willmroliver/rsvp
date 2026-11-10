open Domain.Event
open Common
open Error

let id_codec = Codec.map id_to_int (fun v -> EventID v) Codec.ints

let codec : type a. a field -> (a, string) Codec.t = function
  | ID -> id_codec
  | AddressID -> Location.id_codec
  | Times -> Calendar.span_codec
  | Title -> Codec.identity
  | Body -> Codec.identity
