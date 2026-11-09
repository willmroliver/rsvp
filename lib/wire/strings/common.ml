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
    | BadValue v -> "Bad Value: " ^ v
    | BadField (f, v) -> "Bad Field: " ^ f ^ ", " ^ v
    | DuplicateField s -> "Duplicate: " ^ s
    | MissingRequired s -> "Missing Required: " ^ s
    | Unknown -> "Unknown"

  let many l = Many l

  let field f =
   fun r ->
    r
    |> Result.map_error @@ function
       | BadValue v -> BadField (f, v)
       | BadField _ as v -> v
       | _ -> Unknown

  let rec flatten l =
    List.map (fun l -> match l with Many l -> flatten l | e -> [ e ]) l
    |> List.flatten
end

module Decode = struct
  open Error

  let ( @ ) e f =
    match (e, f) with
    | Many a, Many b -> Many (a @ b)
    | Many a, b | b, Many a -> Many (b :: a)
    | a, b -> Many [ a; b ]

  let ( let+ ) r f = Result.map f r

  let ( and+ ) a b =
    match (a, b) with
    | Ok a, Ok b -> Ok (a, b)
    | Ok _, Error e | Error e, Ok _ -> Error e
    | Error e, Error f -> Error (e @ f)

  let field_result field r f = Result.bind r f |> Error.field field
end

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
        | None -> Error (BadField (name, n)))
    | (name, n) :: tail when name = field.lim -> (
        match int_of_string_opt n with
        | Some n -> args_decode { default with lim = n } tail
        | None -> Error (BadField (name, n)))
    | [] -> Ok default
    | kv :: tail -> Error (BadField kv)
end
