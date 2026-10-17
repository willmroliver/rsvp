let _err request onerr =
 fun status e ->
  match onerr with Some next -> next request e | None -> Dream.empty status

let query ?onerr reader core writer (request : Dream.request) =
  let onerr = _err request onerr in

  match reader @@ Dream.all_queries request with
  | Ok data -> (
      match%lwt Dream.sql request (fun db -> core db data) with
      | Ok res -> writer request res
      | Error e -> onerr `Internal_Server_Error @@ Some e)
  | Error e -> onerr `Internal_Server_Error @@ Some e

let form ?onerr reader core writer (request : Dream.request) =
  let onerr = _err request onerr in

  match%lwt Dream.form request with
  | `Ok fields -> (
      match reader fields with
      | Ok data -> (
          match%lwt Dream.sql request (fun db -> core db data) with
          | Ok res -> writer request res
          | Error e -> onerr `Internal_Server_Error @@ Some e)
      | Error e -> onerr `Internal_Server_Error @@ Some e)
  | _ -> onerr `Bad_Request None

let html page (request : Dream.request) response =
  Dream.html @@ page request response
