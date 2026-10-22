open Domain
open Location
open Petrol
open Petrol.Sqlite3
open Lwt.Syntax

let table, Expr.[ id; line_1; line_2; city; country; post_code ] =
  VersionedSchema.declare_table Version.schema ~name:"address"
    Schema.
      [
        field
          ~constraints:[ primary_key ~auto_increment:true () ]
          "id" ~ty:Type.int;
        field "line_1" ~constraints:[ not_null () ] ~ty:Type.text;
        field "line_2" ~ty:Type.text;
        field "city" ~ty:Type.text;
        field "country" ~ty:Type.text;
        field "post_code" ~constraints:[ not_null () ] ~ty:Type.text;
      ]

let address_decode (id, (line_1, (line_2, (city, (country, (post_code, ()))))))
    =
  let open Proof.Strings in
  let to_opt s = non_empty @@ of_string s in
  Option.product
    (non_empty @@ of_string line_1)
    (non_empty @@ of_string post_code)
  |> Option.map (fun (line_1, post_code) ->
      {
        id = AddressID id;
        line_1 = AddressLine line_1;
        line_2 =
          Option.bind line_2 to_opt |> Option.map (fun v -> AddressLine v);
        city = Option.bind city to_opt |> Option.map (fun v -> City v);
        country = Option.bind country to_opt |> Option.map (fun v -> Country v);
        post_code = PostCode post_code;
      })
  |> Result.ok

module Repo = struct
  open Expr

  type db = Caqti_lwt.connection

  let insert ~(db : db) address =
    let vs = Location.to_strings address in
    Query.insert ~table
      ~values:
        [
          line_1 := s vs.line_1;
          (match vs.line_2 with "" -> unset line_2 | v -> line_2 := s v);
          (match vs.city with "" -> unset city | v -> city := s v);
          (match vs.country with "" -> unset country | v -> country := s v);
          post_code := s vs.post_code;
        ]
    |> Request.make_zero |> exec db
    |> Lwt.map @@ Result.map_error Caqti_error.show

  let get ~(db : db) address_id =
    Query.select
      [
        id; line_1; nullable line_2; nullable city; nullable country; post_code;
      ]
      ~from:table
    |> Query.where (id = i address_id)
    |> Common.make_one db address_decode

  let list ~(db : db) ~off ~lim =
    Query.select
      [
        id; line_1; nullable line_2; nullable city; nullable country; post_code;
      ]
      ~from:table
    |> Query.offset @@ i off
    |> Query.limit @@ i lim
    |> Common.make_many db address_decode

  let delete ~(db : db) address_id =
    Query.delete ~from:table
    |> Query.where (id = i @@ Location.id_to_int address_id)
    |> Common.make_zero db
end
