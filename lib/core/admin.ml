open Common
open Domain
open User

type user_list = { n : int; users : user list }

module type UserRepo = sig
  type db

  val list : db:db -> off:int -> lim:int -> (user list, string) result Lwt.t
  val get : db:db -> user_id -> (user, string) result Lwt.t
end

module Users (UserRepo : UserRepo) = struct
  let list db ({ off; lim } : Listing.args) =
    UserRepo.list ~db ~off ~lim
    |> Lwt.map @@ Result.map (fun users -> { n = List.length users; users })

  let get db id = UserRepo.get ~db id
end
