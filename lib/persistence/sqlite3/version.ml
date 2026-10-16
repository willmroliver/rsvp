open Petrol
open Petrol.Sqlite3

let version = VersionedSchema.version [ 1; 0; 0 ]
let schema = VersionedSchema.init version ~name:"rsvp"
