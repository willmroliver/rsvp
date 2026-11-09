open Common
open Domain.Calendar

let decode v =
  Scanf.sscanf_opt v "%d-%02d-%02dT%02d:%02d:00.0"
    (fun year month day hour minute ->
      make_datetime ~year ~month ~day ~hour ~minute)

let encode v =
  let ({ day; month; year } : date_ints) = date_to_ints v.date in
  let ({ hour; minute } : time_ints) = time_to_ints v.time in
  Format.asprintf "%d-%02d-%02dT%02d:%02d:00.0" year month day hour minute

let ( let+ ) o f = Option.map f o

let ( and+ ) a b =
  match (a, b) with
  | None, None | None, _ | _, None -> None
  | Some a, Some b -> Some (a, b)

let decode_span v =
  Scanf.sscanf_opt v "%s --- %s" (fun from until ->
      let+ from = decode from and+ until = decode until in
      make_span from until)
  |> Option.join

let encode_span { from; until } = encode from ^ " --- " ^ encode until
