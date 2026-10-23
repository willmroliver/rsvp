open Persistence.Sqlite3
open Common
open Core.Auth

let rec post_request (default : args) fields =
  match fields with
  | ("first_name", first_name) :: fields ->
      post_request { default with first_name } fields
  | ("last_name", last_name) :: fields ->
      post_request { default with last_name } fields
  | ("email", email) :: fields -> post_request { default with email } fields
  | [] -> Ok default
  | _ -> Error "unexpected form data"

module Auth = Make (Context) (Db.User.Repo)

let post =
  In.form
  @@ In.pipe
       (post_request { first_name = ""; last_name = ""; email = "" })
       (fun ctx data -> Auth.login ctx data)
  @@ Out.html Page.Auth.post
