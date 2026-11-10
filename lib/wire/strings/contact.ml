open Domain.Contact
open Common
open Codec
open Error

let email_codec =
  {
    encode = email_to_string;
    decode =
      (fun v ->
        parse_email v
        |> Result.map_error (fun e ->
            match e with
            | EmailBadSymbol (c, i) ->
                BadValue
                  (v ^ ": bad symbol " ^ String.of_char c ^ " at "
                 ^ string_of_int i)
            | EmailEmpty -> BadValue v));
  }

let mobile_codec =
  {
    encode = mobile_to_string;
    decode =
      (fun v ->
        parse_mobile v
        |> Result.map_error (fun e ->
            match e with
            | MobileBadSymbol (c, i) ->
                BadValue
                  (v ^ ": bad symbol " ^ String.of_char c ^ " at "
                 ^ string_of_int i)
            | MobileEmpty -> BadValue v));
  }
