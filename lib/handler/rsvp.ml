open Domain
open Core.Rsvp
open Domain.Rsvp
open Wire.Strings.Rsvp

let fields = map (fun (_, name) -> name) Page.Rsvp.fields

let post_request =
  rsvp_decode
    {
      status = NotResponded;
      arriving_from = None;
      returning_to = None;
      transport = Requirement.NoTransport;
      dietary = Requirement.NoDietary;
      plus_ones = 0;
    }

let post_handler db data = Core.Rsvp.rsvp db data |> Lwt.return_ok
let get ?(err = None) = fun req -> Lift.html Page.Rsvp.get req err

let post =
  Lift.form ~onerr:(fun req err -> get ~err req) post_request post_handler
  @@ Lift.html Page.Rsvp.post
