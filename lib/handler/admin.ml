open Persistence.Sqlite3
open Wire.Strings.Common
open Common
open Context

module Users = struct
  module Service = Core.Admin.Users (Context) (Db.User.Repo)

  type error = Wire of Error.t | Core of string | Handler of string

  let map res =
    res
    |> Lwt.map
       @@ Result.map_error (fun e ->
           Dream.log "CORE ERROR - %s" e;
           Core e)

  let wire_error e =
    Dream.log "WIRE ERROR - %s" @@ Error.to_string e;
    Wire e

  (* list *)
  let list_request fields =
    Listing.args_decode { off = 0; lim = 25 } fields
    |> Result.map_error (fun e -> Wire e)

  let list_handler ctx data = Service.list ctx data |> map

  let list =
    In.query
    @@ In.pipe list_request list_handler
    @@ Out.html Page.Admin.Users.list

  (* get *)
  let get_request params =
    Wire.Strings.Admin.Users.get_decode params |> Result.map_error wire_error

  let get_handler ctx data = Service.get ctx data |> map

  let get params =
    In.query ~params
    @@ In.pipe get_request get_handler
    @@ Out.html Page.Admin.Users.get

  (* update *)
  let update_request fields =
    Wire.Strings.User.decode fields |> Result.map_error wire_error

  let update_handler ctx data = Service.update ctx data |> map

  (* delete *)
  let delete_request fields =
    Wire.Strings.Admin.Users.get_decode fields |> Result.map_error wire_error

  let delete_handler ctx data = Service.delete ctx data |> map

  (* post *)
  let post params =
    In.form ~params @@ In.action
    @@ function
    | Update ->
        Some
          (In.pipe update_request update_handler
          @@ Out.redirect Page.Admin.Users.get)
    | Delete ->
        Some
          (In.pipe delete_request delete_handler
          @@ Out.redirect ~target:"/admin/users" Page.Admin.Users.list)
    | _ -> None
end
