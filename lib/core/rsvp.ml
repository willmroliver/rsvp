open Common
open Domain
open Rsvp

module type RsvpRepo = sig
  type db

  val insert : db:db -> rsvp -> (unit, string) result Lwt.t
  val get : db:db -> User.user_id -> (rsvp, string) result Lwt.t
  val list : db:db -> off:int -> lim:int -> (rsvp list, string) result Lwt.t
  val update : db:db -> rsvp -> (unit, string) result Lwt.t
  val delete : db:db -> User.user_id -> (unit, string) result Lwt.t
end

module Make (Context : Context) (RsvpRepo : RsvpRepo with type db = Context.db) =
struct
  open Context

  let insert ctx rsvp = RsvpRepo.insert ~db:ctx.db rsvp
  let get ctx id = RsvpRepo.get ~db:ctx.db id

  let list ctx ({ off; lim } : Common.Listing.args) =
    RsvpRepo.list ~db:ctx.db ~off ~lim

  let update ctx rsvp = RsvpRepo.update ~db:ctx.db rsvp
  let delete ctx id = RsvpRepo.delete ~db:ctx.db id
end
