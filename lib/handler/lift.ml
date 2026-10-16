let ( >> ) f g x = g @@ f x

let using decoder next (entry : 'e -> (string * 'a) list) (exit : 'd -> 'f)
    (lift : ((string * 'a) list -> ('b, 'c) result) -> ('b -> 'd) -> 'e -> 'f) =
  lift decoder next

let identity reader response (request : Dream.request) =
  match reader request with
  | Ok res -> response res
  | _ -> Dream.empty `Bad_Request

let form ?onerr reader next (request : Dream.request) =
  let onerr =
   fun status e ->
    match onerr with Some next -> next request e | None -> Dream.empty status
  in

  match%lwt Dream.form request with
  | `Ok fields -> (
      match%lwt reader fields with
      | Ok res -> next request res
      | Error e -> onerr `Internal_Server_Error @@ Some e)
  | _ -> onerr `Bad_Request None

let html page (request : Dream.request) response =
  Dream.html @@ page request response
