open Common
open Context
open Domain
open Wire.Strings.Common
module Service = Core.Rsvp.Make (Context) (Persistence.Sqlite3.Db.Rsvp.Repo)

module Get = struct
  let get ?(arg = Ok None) = fun req -> Out.html Page.Rsvp.get req arg
  let onerr req err = get

  let create =
    In.query
    @@ In.pipe Result.ok (fun ctx _ ->
        (* @TODO - remove hardcoded user ID once auth flow added *)
        Service.get_opt ctx (User.UserID 1) |> Lwt.map Result.ok)
    @@ Out.html Page.Rsvp.get
end

module Post = struct
  let post_request data = data |> Wire.Strings.Rsvp.decode |> map_wire_error
  let post_handler svc ctx data = svc ctx data |> map_core_error

  let onerr req err =
    Get.get
      ~arg:
        (Option.fold ~none:(Ok None)
           ~some:(function Wire e -> Error (Some e) | _ -> Error None)
           err)
      req

  let create =
    In.form @@ In.action
    @@ function
    | Create ->
        Option.some
        @@ In.pipe ~onerr post_request (post_handler Service.create)
        @@ Out.redirect ~target:"/" Page.Home.get
    | _ -> None
end
