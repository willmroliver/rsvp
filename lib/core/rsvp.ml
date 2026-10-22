open Domain.Rsvp

module type RsvpRepo = sig
  type db

  val insert : db:db -> rsvp -> (unit, string) result Lwt.t
  val get : db:db -> rsvp_id -> (rsvp, string) result Lwt.t
  val list : db:db -> off:int -> lim:int -> (rsvp list, string) result Lwt.t
  val update : db:db -> rsvp -> (unit, string) result Lwt.t
  val delete : db:db -> rsvp_id -> (unit, string) result Lwt.t
end

module Make (RsvpRepo : RsvpRepo) = struct
  let insert db rsvp = RsvpRepo.insert ~db rsvp
  let get db id = RsvpRepo.get ~db id
  let list db ({ off; lim } : Common.Listing.args) = RsvpRepo.list ~db ~off ~lim
  let update db rsvp = RsvpRepo.update ~db rsvp
  let delete db id = RsvpRepo.delete ~db id
end
