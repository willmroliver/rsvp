open Persistence.Sqlite3
open Wire.Strings.Common
open Common
open Context

module Users = struct
  module Service = Core.Admin.Users (Context) (Db.User.Repo)

  module Get = struct
    let list_request fields =
      Listing.args_decode { off = 0; lim = 25 } fields
      |> Result.map_error (fun e -> Wire e)

    let list_handler ctx data = Service.list ctx data |> map_core_error

    let list =
      In.query
      @@ In.pipe list_request list_handler
      @@ Out.html Page.Admin.Users.list

    let get_request params =
      Wire.Strings.Admin.Users.get_decode params |> map_wire_error

    let get_handler ctx data = Service.get ctx data |> map_core_error

    let get params =
      In.query ~params
      @@ In.pipe get_request get_handler
      @@ Out.html Page.Admin.Users.edit

    let create req = Out.html Page.Admin.Users.create req ()
  end

  module Post = struct
    let post_request decoder fields = decoder fields |> map_wire_error
    let post_handler svc ctx data = svc ctx data |> map_core_error

    (* create *)
    let create =
      In.form @@ In.action
      @@ function
      | Create ->
          Option.some
          @@ In.pipe
               (post_request Wire.Strings.User.decode)
               (post_handler Service.create)
          @@ Out.redirect ~target:"/admin/users" Page.Admin.Users.list
      | _ -> None

    (* update/delete *)
    let post params =
      In.form ~params @@ In.action
      @@ function
      | Update ->
          Option.some
          @@ In.pipe
               (post_request Wire.Strings.User.decode)
               (post_handler Service.update)
          @@ Out.redirect Page.Admin.Users.edit
      | Delete ->
          Option.some
          @@ In.pipe
               (post_request Wire.Strings.Admin.Users.get_decode)
               (post_handler Service.delete)
          @@ Out.redirect ~target:"/admin/users" Page.Admin.Users.list
      | _ -> None
  end
end

module Rsvp = struct
  module Service = Core.Admin.Rsvp (Context) (Db.Rsvp.Repo)

  module Get = struct
    let list_request fields =
      Listing.args_decode { off = 0; lim = 25 } fields
      |> Result.map_error (fun e -> Wire e)

    let list_handler ctx data = Service.list ctx data |> map_core_error

    let list =
      In.query
      @@ In.pipe list_request list_handler
      @@ Out.html Page.Admin.Rsvp.list
  end
end
