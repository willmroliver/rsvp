type user_id = UserID of int

let id_to_int = function UserID id -> id

type user = {
  id : user_id;
  first_name : string;
  last_name : string;
  email : Contact.email option;
  mobile : Contact.mobile option;
  address_id : Location.address_id option;
}

type _ field =
  | ID : user_id field
  | FirstName : string field
  | LastName : string field
  | Email : Contact.email option field
  | Mobile : Contact.mobile option field
  | AddressID : Location.address_id option field

type any = Any : _ field -> any

let all =
  [ Any ID; Any FirstName; Any LastName; Any Email; Any Mobile; Any AddressID ]

let get : type a. user -> a field -> a =
 fun user -> function
  | ID -> user.id
  | FirstName -> user.first_name
  | LastName -> user.last_name
  | Email -> user.email
  | Mobile -> user.mobile
  | AddressID -> user.address_id
