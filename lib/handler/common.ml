module In = struct
  let _err request onerr =
   fun status e ->
    match onerr with Some next -> next request e | None -> Dream.empty status

  let query ?onerr ?params reader core writer (request : Dream.request) =
    let onerr = _err request onerr in
    let input = Dream.all_queries request @ Option.value ~default:[] params in

    match reader @@ input with
    | Ok data -> (
        match%lwt Dream.sql request (fun db -> core db data) with
        | Ok res -> writer request res
        | Error e -> onerr `Internal_Server_Error @@ Some e)
    | Error e -> onerr `Internal_Server_Error @@ Some e

  let form ?onerr ?params reader core writer (request : Dream.request) =
    let onerr = _err request onerr in

    match%lwt Dream.form request with
    | `Ok fields -> (
        let fields = fields @ Option.value ~default:[] params in
        match reader fields with
        | Ok data -> (
            match%lwt Dream.sql request (fun db -> core db data) with
            | Ok res -> writer request res
            | Error e -> onerr `Internal_Server_Error @@ Some e)
        | Error e -> onerr `Internal_Server_Error @@ Some e)
    | _ -> onerr `Bad_Request None
end

module Out = struct
  open Page.Common

  let context (request : Dream.request) =
    ({
       csrf = Dream.csrf_token request;
       target = Dream.target request;
       theme = Ctrl.Theme.from_session request;
     }
      : Context.t)

  let html page (request : Dream.request) response =
    Dream.html @@ page (context request) response

  let redirect ?target page (request : Dream.request) response =
    Dream.redirect request
    @@ Option.value ~default:(Dream.target request) target
end
