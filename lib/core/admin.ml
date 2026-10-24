open Common
open Domain
open User
open Rsvp

type user_list = { n : int; users : user list }
type rsvp_list = { n : int; rsvps : (user * rsvp) list }

module type UserRepo = sig
  type db

  val insert : db:db -> user -> (unit, string) result Lwt.t
  val list : db:db -> off:int -> lim:int -> (user list, string) result Lwt.t
  val get : db:db -> user_id -> (user, string) result Lwt.t
  val update : db:db -> user -> (unit, string) result Lwt.t
  val delete : db:db -> user_id -> (unit, string) result Lwt.t
end

module Users (Context : Context) (UserRepo : UserRepo with type db = Context.db) =
struct
  open Context

  let create ctx user = UserRepo.insert ~db:ctx.db user

  let list ctx ({ off; lim } : Listing.args) =
    UserRepo.list ~db:ctx.db ~off ~lim
    |> Lwt.map @@ Result.map (fun users -> { n = List.length users; users })

  let get ctx id = UserRepo.get ~db:ctx.db id
  let update ctx user = UserRepo.update ~db:ctx.db user
  let delete ctx id = UserRepo.delete ~db:ctx.db id
end

module type RsvpRepo = sig
  type db

  val get : db:db -> user_id -> (user * rsvp, string) result Lwt.t

  val list :
    db:db -> off:int -> lim:int -> ((user * rsvp) list, string) result Lwt.t

  val update : db:db -> rsvp -> (unit, string) result Lwt.t
  val delete : db:db -> user_id -> (unit, string) result Lwt.t
end

module Rsvp (Context : Context) (RsvpRepo : RsvpRepo with type db = Context.db) =
struct
  open Context

  let list ctx ({ off; lim } : Common.Listing.args) =
    RsvpRepo.list ~db:ctx.db ~off ~lim
    |> Lwt.map @@ Result.map (fun rsvps -> { n = List.length rsvps; rsvps })

  let get ctx id = RsvpRepo.get ~db:ctx.db id
  let update ctx rsvp = RsvpRepo.update ~db:ctx.db rsvp
  let delete ctx id = RsvpRepo.delete ~db:ctx.db id
end
