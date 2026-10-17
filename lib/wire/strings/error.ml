type decode_error =
  | BadValue of string
  | BadField of (string * string)
  | MissingRequired of string
  | Unknown

let decode_error_to_string e =
  match e with
  | BadValue s -> s
  | BadField (s, t) -> String.concat ", " [ s; t ]
  | MissingRequired s -> s
  | Unknown -> "unknown"
