open Domain.Rsvp
open Petrol
open Petrol.Sqlite3
open Lwt.Syntax

let ( table,
      Expr.
        [
          user_id;
          status;
          arriving_from;
          returning_to;
          transport;
          dietary;
          plus_one;
        ] ) =
  VersionedSchema.declare_table Version.schema ~name:"rsvp"
    Schema.
      [
        field
          ~constraints:
            [
              primary_key ~on_conflict:`REPLACE ();
              foreign_key ~table:User.table ~columns:[ User.id ]
                ~on_update:`CASCADE ~on_delete:`CASCADE ();
            ]
          "user_id" ~ty:Type.int;
        field "status" ~constraints:[ not_null () ] ~ty:Type.text;
        field "arriving_from" ~ty:Type.text;
        field "returning_to" ~ty:Type.text;
        field "transport" ~constraints:[ not_null () ] ~ty:Type.text;
        field "dietary" ~constraints:[ not_null () ] ~ty:Type.text;
        field "plus_one" ~ty:Type.text;
      ]

let rsvp_decode
    ( user_id,
      ( status,
        (arriving_from, (returning_to, (transport, (dietary, (plus_one, ())))))
      ) ) =
  let open Wire.Strings in
  let open Common in
  let open Rsvp in
  let open Proof.Strings in
  (let+ status = status_decode status |> map_err
   and+ arriving_from =
     arriving_from
     |> Option.fold ~none:(Ok None) ~some:(fun v ->
         city_decode v |> Result.map Option.some)
     |> map_err
   and+ returning_to =
     returning_to
     |> Option.fold ~none:(Ok None) ~some:(fun v ->
         city_decode v |> Result.map Option.some)
     |> map_err
   and+ transport = transport_decode transport |> map_err
   and+ dietary = dietary_decode dietary |> map_err
   and+ plus_one =
     plus_one
     |> Option.fold ~none:(Ok None) ~some:(fun v ->
         v |> of_string |> non_empty |> Result.ok)
     |> map_err
   in
   {
     user_id = Domain.User.UserID user_id;
     status;
     arriving_from;
     returning_to;
     transport;
     dietary;
     plus_one;
   })
  |> Result.map_error (fun v -> v |> Error.many |> Error.to_string)

let user_rsvp_decode (user_id, (first_name, (last_name, rsvp))) =
  match
    ( User.user_decode (user_id, (first_name, (last_name, (None, (None, ()))))),
      rsvp_decode (user_id, rsvp) )
  with
  | Ok user, Ok rsvp -> Ok (user, rsvp)
  | Ok _, Error e | Error e, Ok _ -> Error e
  | Error a, Error b -> Error (a ^ ", " ^ b)

module Repo = struct
  type db = Caqti_lwt.connection

  let insert ~(db : db) (rsvp : rsvp) =
    let open Wire.Strings.Rsvp in
    Query.insert ~table
      ~values:
        Expr.
          [
            user_id := i @@ id_to_int rsvp.user_id;
            status := s @@ status_encode rsvp.status;
            Option.fold ~none:(unset arriving_from)
              ~some:(fun v ->
                match v with
                | Domain.Location.City v ->
                    arriving_from := s @@ Proof.Strings.to_string v)
              rsvp.arriving_from;
            Option.fold ~none:(unset returning_to)
              ~some:(fun v ->
                match v with
                | Domain.Location.City v ->
                    returning_to := s @@ Proof.Strings.to_string v)
              rsvp.returning_to;
            transport := s @@ transport_encode rsvp.transport;
            dietary := s @@ dietary_encode rsvp.dietary;
            Option.fold ~none:(unset plus_one)
              ~some:(fun v -> plus_one := s @@ Proof.Strings.to_string v)
              rsvp.plus_one;
          ]
    |> Common.make_zero db

  let select (db : db) =
    let user_pk, user_pk_ref = Expr.as_ ~name:"user_pk" User.id in
    let fname, fname_ref = Expr.as_ ~name:"first_name" User.first_name in
    let lname, lname_ref = Expr.as_ ~name:"last_name" User.last_name in

    Query.select
      [
        user_id;
        fname_ref;
        lname_ref;
        status;
        Expr.nullable arriving_from;
        Expr.nullable returning_to;
        transport;
        dietary;
        Expr.nullable plus_one;
      ]
      ~from:table
    |> Query.join
         (Query.select ~from:User.table Expr.[ user_pk; fname; lname ])
         ~on:Expr.(user_id = user_pk_ref)

  let get ~(db : db) id =
    select db
    |> Query.where Expr.(user_id = i @@ id_to_int id)
    |> Common.make_one db user_rsvp_decode

  let list ~(db : db) ~off ~lim =
    select db
    |> Query.offset @@ Expr.i off
    |> Query.limit @@ Expr.i lim
    |> Common.make_many db user_rsvp_decode

  let update ~(db : db) (rsvp : rsvp) =
    let open Wire.Strings.Rsvp in
    Query.update ~table
      ~set:
        Expr.
          [
            status := s @@ status_encode rsvp.status;
            Option.fold ~none:(unset arriving_from)
              ~some:(function
                | Domain.Location.City v ->
                    arriving_from := s @@ Proof.Strings.to_string v)
              rsvp.arriving_from;
            Option.fold ~none:(unset returning_to)
              ~some:(function
                | Domain.Location.City v ->
                    returning_to := s @@ Proof.Strings.to_string v)
              rsvp.returning_to;
            transport := s @@ transport_encode rsvp.transport;
            dietary := s @@ dietary_encode rsvp.dietary;
            Option.fold ~none:(unset plus_one)
              ~some:(fun v -> plus_one := s @@ Proof.Strings.to_string v)
              rsvp.plus_one;
          ]
    |> Query.where Expr.(user_id = i @@ id_to_int rsvp.user_id)
    |> Common.make_zero db

  let delete ~(db : db) id =
    let open Expr in
    Query.delete ~from:table
    |> Query.where (user_id = i @@ id_to_int id)
    |> Common.make_zero db
end
