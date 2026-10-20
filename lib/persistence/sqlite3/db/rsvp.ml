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

let decode_rsvp
    ( id,
      ( user_id,
        ( status,
          (arriving_from, (returning_to, (transport, (dietary, (plus_one, ())))))
        ) ) ) =
  let open Wire.Strings in
  let open Common in
  let open Error in
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
         |> Option.to_result ~none:(BadValue v)
         |> Result.map Option.some)
     |> map_err
   in
   { status; arriving_from; returning_to; transport; dietary; plus_one })
  |> Result.map_error many
