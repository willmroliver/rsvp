open Petrol
open Petrol.Sqlite3

let make_zero db query =
  query |> Request.make_zero |> exec db
  |> Lwt.map @@ Result.map_error Caqti_error.show

let make_one db decode query =
  query |> Request.make_one |> find db
  |> Lwt.map (fun r ->
      Result.bind (r |> Result.map_error Caqti_error.show) decode)

let make_many db decode query =
  query |> Request.make_many |> collect_list db
  |> Lwt.map (fun r ->
      Result.bind
        (r |> Result.map_error Caqti_error.show)
        (fun v ->
          List.fold_right
            (fun r acc ->
              match (r, acc) with
              | Ok r, Ok acc -> Ok (r :: acc)
              | Error r, Error acc -> Error (acc ^ ", " ^ r)
              | (Error _ as e), _ | _, (Error _ as e) -> e)
            (List.map decode v) (Ok [])))
