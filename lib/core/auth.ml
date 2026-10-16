open Persistence.Sqlite3
open Domain.User

type login_request = { first_name : string; last_name : string; email : string }

let login db { first_name; last_name; email } =
  let store =
   fun conn ->
    match%lwt
      Db.User.insert
        {
          id = UserID 0;
          first_name;
          last_name;
          email = None;
          mobile = None;
          address = None;
        }
        conn
    with
    | Ok _ -> Lwt.return_ok ()
    | _ -> Lwt.return_error ()
  in

  let%lwt res = db store in

  Lwt.return_ok
    {
      first_name = String.uppercase_ascii first_name;
      last_name = String.uppercase_ascii last_name;
      email = String.uppercase_ascii email;
    }
