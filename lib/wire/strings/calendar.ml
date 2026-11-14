open Common
open Error
open Domain.Calendar
open Tool.Codec

let codec =
  {
    encode =
      (fun v ->
        let ({ day; month; year } : date_ints) = date_to_ints v.date in
        let ({ hour; minute } : time_ints) = time_to_ints v.time in
        Format.asprintf "%d-%02d-%02dT%02d:%02d:00.0" year month day hour minute);
    decode =
      (fun v ->
        Scanf.sscanf_opt v "%d-%02d-%02dT%02d:%02d:00.0"
          (fun year month day hour minute ->
            make_datetime ~year ~month ~day ~hour ~minute)
        |> Option.to_result ~none:(BadValue v));
  }

let span_codec =
  {
    encode =
      (fun { from; until } -> codec.encode from ^ " --- " ^ codec.encode until);
    decode =
      (fun v ->
        Scanf.sscanf_opt v "%s --- %s" (fun from until ->
            Decode.(
              let+ from = codec.decode from and+ until = codec.decode until in
              make_span from until))
        |> Option.to_result ~none:(BadValue v)
        |> Result.join);
  }
