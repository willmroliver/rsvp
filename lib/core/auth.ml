open Domain
open User

type login_request = { first_name : string; last_name : string; email : string }
type login_response = login_request

module type UserRepo = sig
  type db

  val insert : db:db -> user -> (unit, string) result Lwt.t
  val get : db:db -> user_id -> (user, string) result Lwt.t
  val list : db:db -> off:int -> lim:int -> (user list, string) result Lwt.t
  val delete : db:db -> user_id -> (unit, string) result Lwt.t
end

module Make (UserRepo : UserRepo) = struct
  let login db { first_name; last_name; email } =
    UserRepo.insert ~db
      {
        id = UserID 0;
        first_name;
        last_name;
        email = Result.to_option @@ Contact.parse_email email;
        mobile = None;
        address = None;
      }
    |> Lwt.map @@ Result.map
       @@ fun () ->
       {
         first_name = String.uppercase_ascii first_name;
         last_name = String.uppercase_ascii last_name;
         email = String.uppercase_ascii email;
       }
end
