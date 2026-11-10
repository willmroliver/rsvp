open Common
open Error

type ('a, 'b) t = { encode : 'a -> 'b; decode : 'b -> ('a, Error.t) result }

let identity = { encode = (fun v -> v); decode = (fun v -> Ok v) }

let maybe ~none t =
  {
    encode = (fun v -> Option.fold ~none ~some:t.encode v);
    decode =
      (fun v ->
        if v = none then Ok None else t.decode v |> Result.map Option.some);
  }

let map f g t =
  {
    encode = (fun v -> t.encode @@ f v);
    decode = (fun v -> t.decode v |> Result.map g);
  }

let ints =
  {
    encode = string_of_int;
    decode =
      (fun v -> int_of_string_opt v |> Option.to_result ~none:(BadValue v));
  }
