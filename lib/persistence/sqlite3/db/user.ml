open Domain
open Petrol
open Petrol.Sqlite3
open Lwt.Syntax

let table, Expr.[ id; first_name; last_name; email; mobile; address_id ] =
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
          ~ty:Type.int;
      ]

let user_decode (id, (first_name, (last_name, (email, (mobile, ()))))) =
  let open Wire.Strings.Common in
  let open Wire.Strings.User in
  (let+ id = Ok (User.UserID id)
   and+ first_name = Ok first_name
   and+ last_name = Ok last_name
   and+ email =
     email
     |> Option.fold ~none:(Ok None) ~some:(fun v ->
         v |> Contact.parse_email |> Result.map Option.some
         |> Result.map_error (fun _ -> Error.BadValue v))
     |> map_err
   and+ mobile =
     mobile
     |> Option.fold ~none:(Ok None) ~some:(fun v ->
         v |> Contact.parse_mobile |> Result.map Option.some
         |> Result.map_error (fun _ -> Error.BadValue v))
     |> map_err
   in
   ({ id; first_name; last_name; email; mobile; address_id = None } : User.user))
  |> Result.map_error (fun v -> Error.many v |> Error.to_string)

module Repo = struct
  type db = Caqti_lwt.connection

  let insert ~(db : db) (user : User.user) =
    Query.insert ~table
      ~values:
        Expr.
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
      [ id; first_name; last_name; Expr.nullable email; Expr.nullable mobile ]
      ~from:table
    |> Query.where Expr.(id = i @@ User.id_to_int user_id)
    |> Request.make_one |> find db
    |> Lwt.map (fun r ->
        Result.bind (r |> Result.map_error Caqti_error.show) user_decode)

  let list ~(db : db) ~off ~lim =
    Query.select
      [ id; first_name; last_name; Expr.nullable email; Expr.nullable mobile ]
      ~from:table
    |> Query.offset @@ Expr.i off
    |> Query.limit @@ Expr.i lim
    |> Request.make_many |> collect_list db
    |> Lwt.map (fun r ->
        r
        |> Result.map_error Caqti_error.show
        |> Result.map @@ List.map user_decode
        |> Result.map (fun v ->
            List.fold_right
              (fun r acc ->
                match (r, acc) with
                | Error v, Error e -> Error (e ^ ", " ^ v)
                | _, (Error _ as e) | (Error _ as e), _ -> e
                | Ok v, Ok l -> Ok (v :: l))
              v (Ok []))
        |> Result.join)

  let update ~(db : db) (user : User.user) =
    let open Expr in
    Query.update ~table
      ~set:
        [
          first_name := s user.first_name;
          last_name := s user.last_name;
          Option.map Contact.email_to_string user.email
          |> Option.fold ~none:(unset email) ~some:(fun v -> email := s v);
          Option.map Contact.mobile_to_string user.mobile
          |> Option.fold ~none:(unset mobile) ~some:(fun v -> mobile := s v);
        ]
    |> Query.where (id = i @@ User.id_to_int user.id)
    |> Request.make_zero |> exec db
    |> Lwt.map @@ Result.map_error Caqti_error.show

  let delete ~(db : db) user_id =
    let open Expr in
    Query.delete ~from:table
    |> Query.where (id = i @@ User.id_to_int user_id)
    |> Request.make_zero |> exec db
    |> Lwt.map @@ Result.map_error Caqti_error.show
end
