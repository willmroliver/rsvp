type user_id = UserID of int

type user = {
  id : user_id;
  first_name : string;
  last_name : string;
  email : Contact.email option;
  mobile : Contact.mobile option;
  address : Location.address option;
}
