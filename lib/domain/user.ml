type user_id = UserID of int

type 'a dict = {
  id : 'a;
  first_name : 'a;
  last_name : 'a;
  email : 'a;
  mobile : 'a;
  address : 'a;
}

type user = {
  id : user_id;
  first_name : string;
  last_name : string;
  email : Contact.email option;
  mobile : Contact.mobile option;
  address : Location.address option;
}

let fold f acc ({ id; first_name; last_name; email; mobile; address } : 'a dict)
    =
  List.fold_right f [ id; first_name; last_name; email; mobile; address ] acc

let map f ({ id; first_name; last_name; email; mobile; address } : 'a dict) =
  ({
     id = f id;
     first_name = f first_name;
     last_name = f last_name;
     email = f email;
     mobile = f mobile;
     address = f address;
   }
    : 'b dict)
