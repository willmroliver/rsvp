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
        field "last_name" ~constraints:[ not_null () ] ~ty:Type.text;
        field "email" ~ty:Type.text;
        field "mobile" ~ty:Type.text;
        field "address_id"
          ~constraints:
            [
              foreign_key ~table:Address.table ~columns:[ Address.id ]
                ~on_update:`CASCADE ();
            ]
          ~ty:Type.text;
      ]

let decode_user (id, (first_name, (last_name, (email, (mobile, ()))))) =
  ({
     id = UserID id;
     first_name;
     last_name;
     email =
       Option.bind email (fun v -> Contact.parse_email v |> Result.to_option);
     mobile =
       Option.bind mobile (fun v -> Contact.parse_mobile v |> Result.to_option);
     address = None;
   }
    : User.user)

module Repo = struct
  open Expr

  type db = Caqti_lwt.connection

  let insert ~(db : db) (user : User.user) =
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
    |> Lwt.map @@ Result.map_error Caqti_error.show

  let get ~(db : db) user_id =
    Query.select
      [ id; first_name; last_name; nullable email; nullable mobile ]
      ~from:table
    |> Query.where (id = i @@ User.id_to_int user_id)
    |> Request.make_one |> find db
    |> Lwt.map (fun r ->
        r |> Result.map_error Caqti_error.show |> Result.map decode_user)

  let list ~(db : db) ~off ~lim =
    Query.select
      [ id; first_name; last_name; nullable email; nullable mobile ]
      ~from:table
    |> Query.offset @@ i off
    |> Query.limit @@ i lim
    |> Request.make_many |> collect_list db
    |> Lwt.map (fun r ->
        r
        |> Result.map_error Caqti_error.show
        |> Result.map @@ List.map decode_user)

  let delete ~(db : db) user_id =
    Query.delete ~from:table
    |> Query.where (id = i @@ User.id_to_int user_id)
    |> Request.make_zero |> exec db
    |> Lwt.map @@ Result.map_error Caqti_error.show
end
