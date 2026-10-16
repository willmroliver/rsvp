open Petrol
open Petrol.Sqlite3

let _ = Db.User.table

let run uri =
  let open Lwt_result.Syntax in
  let* conn = Caqti_lwt_unix.connect @@ Uri.of_string uri in

  VersionedSchema.initialise Version.schema conn
