open Domain.Contact
open Common
open Error

let email_decode v =
  parse_email v
  |> Result.map_error (fun e ->
      match e with
      | EmailBadSymbol (c, i) ->
          BadValue
            (v ^ ": bad symbol " ^ String.of_char c ^ " at " ^ string_of_int i)
      | EmailEmpty -> BadValue v)

let email_encode v = Basic.option_encode email_to_string v

let mobile_decode v =
  parse_mobile v
  |> Result.map_error (fun e ->
      match e with
      | MobileBadSymbol (c, i) ->
          BadValue
            (v ^ ": bad symbol " ^ String.of_char c ^ " at " ^ string_of_int i)
      | MobileEmpty -> BadValue v)

let mobile_encode v = Basic.option_encode mobile_to_string v
