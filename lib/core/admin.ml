open Common
open Domain
open User

type page = { n : int; users : user list }

module type UserRepo = sig
  type db

  val list : db:db -> off:int -> lim:int -> (user list, string) result Lwt.t
end

module Make (UserRepo : UserRepo) = struct
  let users db ({ off; lim } : list_args) =
    UserRepo.list ~db ~off ~lim
    |> Lwt.map @@ Result.map (fun users -> { n = List.length users; users })
end
