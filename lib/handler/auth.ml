let rec post_request default fields =
  let open Core.Auth in
  match fields with
  | ("first_name", first_name) :: fields ->
      post_request { default with first_name } fields
  | ("last_name", last_name) :: fields ->
      post_request { default with last_name } fields
  | ("email", email) :: fields -> post_request { default with email } fields
  | [] -> Ok default
  | _ -> Error ()

let post_handler db data =
  match post_request { first_name = ""; last_name = ""; email = "" } data with
  | Ok data -> Core.Auth.login db data
  | _ -> Lwt.return_error ()

let post req =
  ((Lift.form @@ post_handler (Dream.sql req)) @@ Lift.html Page.Auth.post) req
