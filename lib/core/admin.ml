open Common
open Domain
open User

type user_list = { n : int; users : user list }

module type UserRepo = sig
  type db

  val list : db:db -> off:int -> lim:int -> (user list, string) result Lwt.t
  val get : db:db -> user_id -> (user, string) result Lwt.t
  val update : db:db -> user -> (unit, string) result Lwt.t
  val delete : db:db -> user_id -> (unit, string) result Lwt.t
end

module Users (Context : Context) (UserRepo : UserRepo with type db = Context.db) =
struct
  open Context

  let list ctx ({ off; lim } : Listing.args) =
    UserRepo.list ~db:ctx.db ~off ~lim
    |> Lwt.map @@ Result.map (fun users -> { n = List.length users; users })

  let get ctx id = UserRepo.get ~db:ctx.db id
  let update ctx user = UserRepo.update ~db:ctx.db user
  let delete ctx id = UserRepo.delete ~db:ctx.db id
end
