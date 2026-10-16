open Domain
open Petrol
open Petrol.Sqlite3
open Lwt.Syntax

let table, Expr.[ id; first_name; last_name; email; mobile; address ] =
  VersionedSchema.declare_table Version.schema ~name:"user"
    Schema.
      [
        field
          ~constraints:[ primary_key ~auto_increment:true () ]
          "id" ~ty:Type.int;
        field "first_name" ~constraints:[ not_null () ] ~ty:Type.text;
        field "last_name" ~ty:Type.text;
        field "email" ~ty:Type.text;
        field "mobile" ~ty:Type.text;
        field "address" ~ty:Type.text;
      ]

let id_to_int id = match id with User.UserID id -> id

let insert (user : User.user) db =
  let open Expr in
  Query.insert ~table
    ~values:
      [
        first_name := s user.first_name;
        last_name := s user.last_name;
        Option.map Contact.email_to_string user.email
        |> Option.fold ~none:(unset email) ~some:(fun v -> email := s v);
        Option.map Contact.mobile_to_string user.mobile
        |> Option.fold ~none:(unset mobile) ~some:(fun v -> mobile := s v);
      ]
  |> Request.make_zero |> exec db

let get (user_id : int) db =
  Query.select [ id; first_name; last_name; email; mobile; address ] ~from:table
  |> Query.where Expr.(id = i user_id)
  |> Request.make_one |> find db

let list ~off ~lim db =
  Query.select [ id; first_name; last_name; email; mobile; address ] ~from:table
  |> Query.offset @@ Expr.i off
  |> Query.limit @@ Expr.i lim
  |> Request.make_many |> collect_list db
