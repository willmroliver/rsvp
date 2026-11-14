open Common
open Error
open Tool

let int_codec =
  Codec.
    {
      encode = string_of_int;
      decode =
        (fun v -> int_of_string_opt v |> Option.to_result ~none:(BadValue v));
    }

module GADT = struct
  module type Record = sig
    type t
    type _ field
    type any = Any : _ field -> any

    val all : any list
    val get : t -> 'a field -> 'a
  end

  module type Wire = sig
    type t
    type _ field

    val get_field_codec : 'a field -> ('a, string, Error.t) Codec.t
    val field_name : 'a field -> string
  end

  module type S = sig
    type t
    type _ field

    val field_name : 'a field -> string
    val field_encode : t -> 'a field -> string
    val field_decode : 'a field -> string -> ('a, Error.t) result
    val encode : t -> (string * string) list
    val decode : (string * string) list -> 'a field -> ('a, Error.t) result
  end

  module Make
      (Record : Record)
      (Wire : Wire with type t := Record.t and type 'a field := 'a Record.field) :
    S with type t := Record.t and type 'a field := 'a Record.field = struct
    let field_name = Wire.field_name

    let field_encode : type a. Record.t -> a Record.field -> string =
     fun t f -> (Wire.get_field_codec f).encode (Record.get t f)

    let field_decode : type a. a Record.field -> string -> (a, Error.t) result =
     fun f s -> (Wire.get_field_codec f).decode s

    let encode : type a. Record.t -> (string * string) list =
     fun t ->
      List.map
        (function Record.Any f -> (Wire.field_name f, field_encode t f))
        Record.all

    let decode : type a.
        (string * string) list -> a Record.field -> (a, Error.t) result =
     fun data f ->
      let name = Wire.field_name f in
      List.filter_map (fun (k, v) -> if k = name then Some v else None) data
      |> function
      | v :: [] -> v |> field_decode f
      | [] -> Error (MissingRequired name)
      | l -> Error (DuplicateField name)
  end
end
