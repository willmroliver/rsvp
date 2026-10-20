module Error = struct
  type t =
    | Many of t list
    | BadValue of string
    | BadField of (string * string)
    | DuplicateField of string
    | MissingRequired of string
    | Unknown

  let rec to_string e =
    match e with
    | Many s -> String.concat ", " @@ List.map to_string s
    | BadValue s -> "Bad Value: " ^ s
    | BadField (s, t) -> "Bad Field: " ^ String.concat ", " [ s; t ]
    | DuplicateField s -> "Duplicate: " ^ s
    | MissingRequired s -> "Missing Required: " ^ s
    | Unknown -> "Unknown"

  let many l = Many l

  let rec flatten l =
    List.map (fun l -> match l with Many l -> flatten l | e -> [ e ]) l
    |> List.flatten
end

let map_err e = Result.map_error List.singleton e
let ( let+ ) r f = Result.map f r

let ( and+ ) a b =
  match (a, b) with
  | Ok a, Ok b -> Ok (a, b)
  | Ok _, Error e | Error e, Ok _ -> Error e
  | Error e, Error f -> Error (e @ f)

module Fields = struct
  open Error

  let one fields name =
    match List.partition (fun (f, v) -> f = name) fields with
    | [ (f, v) ], fields -> (Ok v, fields)
    | [], fields -> (Error (MissingRequired name), fields)
    | l, fields -> (Error (DuplicateField "Duplicates"), fields)

  let one_opt fields name =
    match List.partition (fun (f, v) -> f = name) fields with
    | [ (_, "") ], fields | [], fields -> (Ok None, fields)
    | [ (_, v) ], fields -> (Ok (Some v), fields)
    | l, fields -> (Error (DuplicateField "Duplicates"), fields)

  let all fields name =
    match List.partition (fun (f, v) -> f = name) fields with
    | [], fields -> (Error (MissingRequired name), fields)
    | got, fields -> (Ok (List.map snd got), fields)

  let all_opt fields name =
    match List.partition (fun (f, v) -> f = name) fields with
    | [], fields -> (Ok None, fields)
    | got, fields -> (Ok (Some (List.map snd got)), fields)
end

module Listing = struct
  open Core.Common
  open Error

  let field = ({ off = "from"; lim = "size" } : string Listing.args_dict)

  let rec args_decode (default : Listing.args) = function
    | (name, n) :: tail when name = field.off -> (
        match int_of_string_opt n with
        | Some n -> args_decode { default with off = n } tail
        | None -> Error (BadValue n))
    | (name, n) :: tail when name = field.lim -> (
        match int_of_string_opt n with
        | Some n -> args_decode { default with lim = n } tail
        | None -> Error (BadValue n))
    | [] -> Ok default
    | kv :: tail -> Error (BadField kv)
end
