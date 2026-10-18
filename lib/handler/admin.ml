open Persistence.Sqlite3
open Core.Common
open Common

module Users = struct
  module Service = Core.Admin.Users (Db.User.Repo)

  type error = Wire of Wire.Strings.Error.decode_error | Core of string

  let map res =
    res
    |> Lwt.map
       @@ Result.map_error (fun e ->
           Dream.log "CORE ERROR - %s" e;
           Core e)

  (* list *)
  let list_request fields =
    Wire.Strings.Listing.args_decode { off = 0; lim = 25 } fields
    |> Result.map_error (fun e -> Wire e)

  let list_handler db data = Service.list db data |> map
  let list = In.query list_request list_handler @@ Out.html Page.Admin.list

  (* get *)
  let get_handler db data = Service.get db data |> map

  let get_request params =
    let open Wire.Strings in
    Admin.Users.get_decode params
    |> Result.map_error (fun e ->
        Dream.log "WIRE ERROR - %s" @@ Error.decode_error_to_string e;
        Wire e)

  let get params =
    In.query ~params get_request get_handler @@ Out.html Page.Admin.get
end
