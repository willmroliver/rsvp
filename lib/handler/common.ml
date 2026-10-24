open Core.Common
open Wire.Strings.Common

type error = Wire of Error.t | Core of string | Handler of string

let map_core_error res =
  res
  |> Lwt.map
     @@ Result.map_error (fun e ->
         Dream.log "CORE ERROR - %s" e;
         Core e)

let map_wire_error e =
  e
  |> Result.map_error (fun e ->
      Dream.log "WIRE ERROR - %s" @@ Error.to_string e;
      Wire e)

module Context = struct
  type db = Caqti_lwt.connection

  type t = {
    db : db;
    log : 'a. ('a, Format.formatter, unit, unit) format4 -> 'a;
  }

  let make db = { db; log = Dream.log }
end

module In = struct
  type fields = (string * string) list
  type pipeline = fields -> Dream.handler

  let _err request onerr =
   fun status e ->
    match onerr with Some next -> next request e | None -> Dream.empty status

  let pipe ?onerr reader core writer =
   fun fields req ->
    match reader fields with
    | Ok data -> (
        match%lwt Dream.sql req (fun db -> core (Context.make db) data) with
        | Ok res -> writer req res
        | Error e -> _err req onerr `Internal_Server_Error @@ Some e)
    | Error e -> _err req onerr `Internal_Server_Error @@ Some e

  let query ?(params = []) (pipeline : pipeline) =
   fun req -> req |> pipeline @@ Dream.all_queries req @ params

  let form ?(params = []) (pipeline : pipeline) =
   fun req ->
    match%lwt Dream.form req with
    | `Ok fields -> req |> pipeline @@ fields @ params
    | _ ->
        Dream.log "WIRE ERROR - Failed to parse form";
        _err req None `Bad_Request None

  let action (map : Action.kind -> pipeline option) =
   fun fields req ->
    let kind, fields = Wire.Strings.Action.partition fields in
    match kind with
    | Ok kind -> (
        match map kind with
        | Some pipeline -> pipeline fields req
        | None -> _err req None `Bad_Request None)
    | Error _ ->
        Dream.log "WIRE ERROR - Unrecognised action kind";
        _err req None `Bad_Request None
end

module Out = struct
  let context (request : Dream.request) =
    ({
       csrf = Dream.csrf_token request;
       target = Dream.target request;
       theme = Ctrl.Theme.from_session request;
       log = Dream.log;
     }
      : Page.Common.Context.t)

  let html page (request : Dream.request) response =
    Dream.html @@ page (context request) response

  let redirect ?target page (request : Dream.request) response =
    Dream.redirect request
    @@ Option.value ~default:(Dream.target request) target
end
