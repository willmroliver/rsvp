open Common
open Context
open Domain
open Wire.Strings.Common

module Rsvp = struct
  module Service = Core.Rsvp.Make (Context) (Persistence.Sqlite3.Db.Rsvp.Repo)

  type error = Core of string | Wire of Error.t

  let get ?(err = None) = fun req -> Out.html Page.Rsvp.get req err

  let post_request data =
    data |> Wire.Strings.Rsvp.decode
    |> Result.map_error (fun e ->
        Dream.log "WIRE ERROR: %s" @@ Error.to_string e;
        Wire e)

  let post_handler ctx data =
    Service.insert ctx data
    |> Lwt.map
       @@ Result.map_error (fun e ->
           Dream.log "CORE ERROR: %s" e;
           Core e)

  let post =
    In.form @@ In.action
    @@ function
    | Create ->
        Option.some
        @@ In.pipe
             ~onerr:(fun req err ->
               get
                 ~err:
                   (Option.bind err (fun err ->
                        match err with Core e -> None | Wire e -> Some e))
                 req)
             post_request post_handler
        @@ Out.redirect Page.Home.get
    | _ -> None

  open Core.Common.Action
end
