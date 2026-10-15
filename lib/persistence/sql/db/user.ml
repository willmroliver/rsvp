open Petrol
open Petrol.Sqlite3

let table, Expr.[ id; name ] =
  VersionedSchema.declare_table Shared.schema ~name:"user"
    Schema.
      [
        field
          ~constraints:[ primary_key ~auto_increment:true () ]
          "id" ~ty:Type.int;
        field "name" ~ty:Type.text;
      ]
