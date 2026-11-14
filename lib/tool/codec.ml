type ('a, 'b, 'e) t = { encode : 'a -> 'b; decode : 'b -> ('a, 'e) result }

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

let bind f g t =
  {
    encode = (fun v -> t.encode @@ f v);
    decode = (fun v -> Result.bind (t.decode v) g);
  }

let product t u =
  {
    encode = (fun (v, w) -> (t.encode v, u.encode w));
    decode = (fun (v, w) -> u.decode w |> Result.product @@ t.decode v);
  }

let list ?(empty = Ok []) t =
  {
    encode = (fun l -> List.map t.encode l);
    decode =
      (fun l ->
        List.map t.decode l
        |> List.fold_left
             (fun acc r ->
               match (acc, r) with
               | Ok l, Ok r -> Ok (r :: l)
               | Error l, Error e -> Error (e :: l)
               | (Error _ as l), _ -> l
               | _, Error e -> Error [ e ])
             (Ok []));
  }

let const t c err =
  {
    encode = (fun () -> t.encode c);
    decode = (fun v -> if t.decode v = Ok c then Ok () else Error (err v));
  }

let identity = { encode = (fun v -> v); decode = (fun v -> Ok v) }
