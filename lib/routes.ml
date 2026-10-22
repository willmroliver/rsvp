let rec params ?(data = []) keys handler request =
  match keys with
  | param :: keys ->
      params
        ~data:((param, Dream.param request param) :: data)
        keys handler request
  | [] -> handler data request

let start () =
  let db_uri =
    match Sys.getenv_opt "DB" with
    | Some db -> db
    | None -> "sqlite3:./data/db.sqlite3"
  in

  (match Lwt_main.run (Persistence.Sqlite3.Migrate.run db_uri) with
  | Ok () -> ()
  | Error (`Newer_version_than_supported _) ->
      failwith "newer version than supported"
  | Error (#Caqti_error.t as e) -> failwith (Caqti_error.show e));

  Dream.run ~interface:"0.0.0.0" ~port:8080
  @@ Dream.sql_pool db_uri @@ Dream.logger @@ Dream.memory_sessions
  @@ Ctrl.Theme.middleware
  @@ Dream.router
       [
         (* static *)
         Dream.get "/static/**" @@ Dream.static "static";
         Dream.get "/mdi/**" @@ Dream.static "node_modules/@mdi/font";
         (* root *)
         Dream.get "/" Handler.Home.get;
         (* auth *)
         Dream.get "/login" Handler.Home.get;
         Dream.post "/login" Handler.Auth.post;
         (* admin *)
         Dream.get "/admin/users" Handler.Admin.Users.list;
         Dream.get "/admin/users/:id" @@ params [ "id" ] Handler.Admin.Users.get;
         Dream.post "/admin/users/:id"
         @@ params [ "id" ] Handler.Admin.Users.post;
         (* rsvp *)
         Dream.get "/rsvp" Handler.Rsvp.Rsvp.get;
         Dream.post "/rsvp" Handler.Rsvp.Rsvp.post;
         (* ctrl *)
         Dream.post "/theme" (fun req -> Dream.redirect req (Dream.target req));
       ]
