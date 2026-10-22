open Domain.Rsvp
open Petrol
open Petrol.Sqlite3
open Lwt.Syntax

let ( table,
      Expr.
        [
          id;
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
          ~constraints:[ primary_key ~auto_increment:true () ]
          "id" ~ty:Type.int;
        field
          ~constraints:
            [
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
    ( id,
      ( user_id,
        ( status,
          (arriving_from, (returning_to, (transport, (dietary, (plus_one, ())))))
        ) ) ) =
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
         v |> of_string |> non_empty
         |> Option.to_result ~none:(Error.BadValue v)
         |> Result.map Option.some)
     |> map_err
   in
   {
     id = RsvpID id;
     user_id = Domain.User.UserID user_id;
     status;
     arriving_from;
     returning_to;
     transport;
     dietary;
     plus_one;
   })
  |> Result.map_error (fun v -> v |> Error.many |> Error.to_string)

module Repo = struct
  type db = Caqti_lwt.connection

  let insert ~(db : db) (rsvp : rsvp) =
    let open Wire.Strings.Rsvp in
    Query.insert ~table
      ~values:
        Expr.
          [
            id := i @@ id_to_int rsvp.id;
            user_id := i @@ Domain.User.id_to_int rsvp.user_id;
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
            plus_one :=
              s
              @@ (rsvp.plus_one
                 |> Option.map Proof.Strings.to_string
                 |> Option.value ~default:"");
          ]
    |> Common.make_zero db

  let get ~(db : db) rsvp_id =
    Query.select
      [
        id;
        user_id;
        status;
        Expr.nullable arriving_from;
        Expr.nullable returning_to;
        transport;
        dietary;
        Expr.nullable plus_one;
      ]
      ~from:table
    |> Query.where Expr.(id = i @@ id_to_int rsvp_id)
    |> Common.make_one db rsvp_decode

  let list ~(db : db) ~off ~lim =
    Query.select
      [
        id;
        user_id;
        status;
        Expr.nullable arriving_from;
        Expr.nullable returning_to;
        transport;
        dietary;
        Expr.nullable plus_one;
      ]
      ~from:table
    |> Query.offset @@ Expr.i off
    |> Query.limit @@ Expr.i lim
    |> Common.make_many db rsvp_decode

  let update ~(db : db) (rsvp : rsvp) =
    let open Wire.Strings.Rsvp in
    Query.update ~table
      ~set:
        Expr.
          [
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
            plus_one :=
              s
              @@ (rsvp.plus_one
                 |> Option.map Proof.Strings.to_string
                 |> Option.value ~default:"");
          ]
    |> Query.where Expr.(id = i @@ id_to_int rsvp.id)
    |> Common.make_zero db

  let delete ~(db : db) rsvp_id =
    let open Expr in
    Query.delete ~from:table
    |> Query.where (id = i @@ id_to_int rsvp_id)
    |> Common.make_zero db
end
