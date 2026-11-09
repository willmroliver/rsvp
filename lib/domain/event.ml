type event_id = EventID of int

type event = {
  id : event_id;
  address_id : Location.address_id;
  times : Calendar.span;
  title : string;
  body : string;
}

type _ field =
  | ID : event_id field
  | AddressID : Location.address_id field
  | Times : Calendar.span field
  | Title : string field
  | Body : string field

type any = Any : _ field -> any

let get : type a. event -> a field -> a =
 fun e -> function
  | ID -> e.id
  | AddressID -> e.address_id
  | Times -> e.times
  | Title -> e.title
  | Body -> e.body

let id_to_int = function EventID id -> id
