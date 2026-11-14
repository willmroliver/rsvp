open Proof.Strings

type status = Coming | MaybeComing | NotComing

let fold_status f acc = List.fold_right f [ Coming; MaybeComing; NotComing ] acc
let id_to_int = User.id_to_int

type rsvp = {
  user_id : User.user_id;
  status : status;
  arriving_from : Location.city option;
  returning_to : Location.city option;
  transport : Requirement.transport;
  dietary : Requirement.dietary;
  plus_one : string_proof non_empty t option;
}

type _ field =
  | UserID : User.user_id field
  | Status : status field
  | ArrivingFrom : Location.city option field
  | ReturningTo : Location.city option field
  | Transport : Requirement.transport field
  | Dietary : Requirement.dietary field
  | PlusOne : string_proof non_empty t option field

type any = Any : _ field -> any

let all =
  [
    Any UserID;
    Any Status;
    Any ArrivingFrom;
    Any ReturningTo;
    Any Transport;
    Any Dietary;
    Any PlusOne;
  ]

let get : type a. rsvp -> a field -> a =
 fun rsvp -> function
  | UserID -> rsvp.user_id
  | Status -> rsvp.status
  | ArrivingFrom -> rsvp.arriving_from
  | ReturningTo -> rsvp.returning_to
  | Transport -> rsvp.transport
  | Dietary -> rsvp.dietary
  | PlusOne -> rsvp.plus_one
