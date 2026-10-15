type string_proof = unit
type 'proof t = string

let of_string (s : string) = (s : unit t)
let to_string (s : 'proof t) = (s : string)

type 'p non_empty
type 'p alpha
type 'p numeric
type 'p alpha_numeric

let check pred s = if pred s then Some s else None
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let non_empty = check @@ fun s -> s <> ""
let alpha = check @@ String.for_all is_alpha
let numeric = check @@ String.for_all is_digit
let alpha_numeric = check @@ String.for_all @@ fun c -> is_alpha c || is_digit c
