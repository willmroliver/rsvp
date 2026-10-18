type decode_error =
  | BadValue of string
  | BadField of (string * string)
  | MissingRequired of string
  | Unknown

let decode_error_to_string e =
  match e with
  | BadValue s -> "Bad Value: " ^ s
  | BadField (s, t) -> "Bad Field: " ^ String.concat ", " [ s; t ]
  | MissingRequired s -> "Missing Required: " ^ s
  | Unknown -> "Unknown"
