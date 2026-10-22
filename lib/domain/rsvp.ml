open Proof.Strings

type status = Coming | MaybeComing | NotComing

let fold_status f acc = List.fold_right f [ Coming; MaybeComing; NotComing ] acc

type 'a dict = {
  id : 'a;
  user_id : 'a;
  status : 'a;
  arriving_from : 'a;
  returning_to : 'a;
  transport : 'a;
  dietary : 'a;
  plus_one : 'a;
}

type rsvp_id = RsvpID of int

let id_to_int = function RsvpID id -> id

type rsvp = {
  id : rsvp_id;
  user_id : User.user_id;
  status : status;
  arriving_from : Location.city option;
  returning_to : Location.city option;
  transport : Requirement.transport;
  dietary : Requirement.dietary;
  plus_one : string_proof non_empty t option;
}

let fold f acc
    ({
       id;
       user_id;
       status;
       arriving_from;
       returning_to;
       transport;
       dietary;
       plus_one;
     } :
      'a dict) =
  List.fold_right f
    [
      id;
      user_id;
      status;
      arriving_from;
      returning_to;
      transport;
      dietary;
      plus_one;
    ]
    acc

let map f
    ({
       id;
       user_id;
       status;
       arriving_from;
       returning_to;
       transport;
       dietary;
       plus_one;
     } :
      'a dict) =
  ({
     id = f id;
     user_id = f user_id;
     status = f status;
     arriving_from = f arriving_from;
     returning_to = f returning_to;
     transport = f transport;
     dietary = f dietary;
     plus_one = f plus_one;
   }
    : 'b dict)

let zip (a : 'a dict) (b : 'b dict) =
  ({
     id = (a.id, b.id);
     user_id = (a.user_id, b.user_id);
     status = (a.status, b.status);
     arriving_from = (a.arriving_from, b.arriving_from);
     returning_to = (a.returning_to, b.returning_to);
     transport = (a.transport, b.transport);
     dietary = (a.dietary, b.dietary);
     plus_one = (a.plus_one, b.plus_one);
   }
    : ('a * 'b) dict)
