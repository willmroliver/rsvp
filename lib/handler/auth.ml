open Persistence.Sqlite3

let rec post_request default fields =
  let open Core.Auth in
  match fields with
  | ("first_name", first_name) :: fields ->
      post_request { default with first_name } fields
  | ("last_name", last_name) :: fields ->
      post_request { default with last_name } fields
  | ("email", email) :: fields -> post_request { default with email } fields
  | [] -> Ok default
  | _ -> Error "unexpected form data"

module Auth = Core.Auth.Make (Db.User.Repo)

let post =
  Lift.form
    (post_request { first_name = ""; last_name = ""; email = "" })
    Auth.login
  @@ Lift.html Page.Auth.post
