open Domain.Event
open Common
open Error
open Tool

let id_codec = Codec.map id_to_int (fun v -> EventID v) Basic.int_codec

let codec : type a. a field -> (a, string, Error.t) Codec.t = function
  | ID -> id_codec
  | AddressID -> Location.id_codec
  | Times -> Calendar.span_codec
  | Title -> Codec.identity
  | Body -> Codec.identity
