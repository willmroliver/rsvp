open Proof.Strings

type email = Email of string_proof non_empty t
type email_error = Empty | BadSymbol of char * int

val parse_email : string -> (email, email_error) result
val email_to_string : email -> string

type mobile = Mobile of string_proof non_empty t

type mobile_error =
  | Empty
  | BadSymbol of char * int
  | Malformed of string * char list

val parse_mobile : string -> (mobile, mobile_error) result
val mobile_to_string : mobile -> string
