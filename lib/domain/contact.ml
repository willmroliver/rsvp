open Proof.Strings

type email = Email of string_proof non_empty t
type email_error = Empty | BadSymbol of char * int

let parse_email s =
  let n = String.length s in
  match String.index s '@' with
  | 0 | 1 -> Error (BadSymbol ('@', 0))
  | i when i >= n - 2 -> Error (BadSymbol ('@', i))
  | _ ->
      Option.fold ~none:(Error Empty)
        ~some:(fun s -> Ok (Email s))
        (non_empty @@ of_string s)

let email_to_string e = match e with Email s -> to_string s

type mobile = Mobile of string_proof non_empty t

type mobile_error =
  | Empty
  | BadSymbol of char * int
  | Malformed of string * char list

let parse_mobile s =
  match String.find_first ~sub:"+" s with
  | Some 0 | None -> (
      match non_empty @@ of_string s with
      | Some v -> (
          match numeric @@ of_string @@ String.take_first 1 s with
          | Some _ -> Ok (Mobile v)
          | None -> Error Empty)
      | None -> Error Empty)
  | Some n -> Error (BadSymbol ('+', n))

let mobile_to_string m = match m with Mobile m -> to_string m
