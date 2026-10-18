type user_id = UserID of int

let id_to_int = function UserID id -> id

type 'a dict = {
  id : 'a;
  first_name : 'a;
  last_name : 'a;
  email : 'a;
  mobile : 'a;
  address_id : 'a;
}

type user = {
  id : user_id;
  first_name : string;
  last_name : string;
  email : Contact.email option;
  mobile : Contact.mobile option;
  address_id : Location.address_id option;
}

let fold f acc
    ({ id; first_name; last_name; email; mobile; address_id } : 'a dict) =
  List.fold_right f [ id; first_name; last_name; email; mobile; address_id ] acc

let map f ({ id; first_name; last_name; email; mobile; address_id } : 'a dict) =
  ({
     id = f id;
     first_name = f first_name;
     last_name = f last_name;
     email = f email;
     mobile = f mobile;
     address_id = f address_id;
   }
    : 'b dict)

let zip (a : 'a dict) (b : 'b dict) =
  ({
     id = (a.id, b.id);
     first_name = (a.first_name, b.first_name);
     last_name = (a.last_name, b.last_name);
     email = (a.email, b.email);
     mobile = (a.mobile, b.mobile);
     address_id = (a.address_id, b.address_id);
   }
    : ('a * 'b) dict)
