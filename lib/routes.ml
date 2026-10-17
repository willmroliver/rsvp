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
  @@ Theme.middleware
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
         (* rsvp *)
         Dream.get "/rsvp" Handler.Rsvp.get;
         Dream.post "/rsvp" Handler.Rsvp.post;
         (* ctrl *)
         Dream.post "/theme" (fun req -> Dream.redirect req (Dream.target req));
         Dream.get "/param/:of" (fun req -> Dream.html (Dream.param req "of"));
         Dream.post "/echo" (fun req ->
             let%lwt body = Dream.body req in
             Dream.respond
               ~headers:[ ("Content-Type", "application/octet-stream") ]
               body);
       ]
