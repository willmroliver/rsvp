open Common
open Domain
open Rsvp

module type RsvpRepo = sig
  type db

  val insert : db:db -> rsvp -> (unit, string) result Lwt.t
  val get : db:db -> User.user_id -> (User.user * rsvp, string) result Lwt.t
end

module Make (Context : Context) (RsvpRepo : RsvpRepo with type db = Context.db) =
struct
  open Context

  let create ctx rsvp = RsvpRepo.insert ~db:ctx.db rsvp
  let get ctx id = RsvpRepo.get ~db:ctx.db id

  let get_opt ctx id =
    get ctx id
    |> Lwt.map
       @@ Result.fold
          (* @TODO - model db failures properly, such that core can identify a missing record versus a real error *)
            ~error:(fun _ -> Result.ok None)
            ~ok:(fun (user, rsvp) -> Result.ok @@ Option.some rsvp)
end
