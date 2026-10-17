open Persistence.Sqlite3
open Core.Admin
open Core.Common
module Admin = Core.Admin.Make (Db.User.Repo)

module Users = struct
  open Wire.Strings.Error

  type error = Decode of decode_error | Core of string

  let list_request fields =
    Wire.Strings.Common.list_args_decode { off = 0; lim = 25 } fields
    |> Result.map_error (fun e -> Decode e)

  let list_handler db data =
    Admin.users db data
    |> Lwt.map
       @@ Result.map_error (fun e ->
           Dream.log "ERROR - %s" e;
           Core e)

  let list = Lift.query list_request list_handler @@ Lift.html Page.Admin.get
end
