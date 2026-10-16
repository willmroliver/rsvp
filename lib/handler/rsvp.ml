open Domain
open Core.Rsvp
open Domain.Rsvp
open Wire.Form.Rsvp

let fields = map (fun (_, name) -> name) Page.Rsvp.fields

let post_handler data =
  rsvp_decode
    {
      status = NotResponded;
      arriving_from = None;
      returning_to = None;
      transport = Requirement.NoTransport;
      dietary = Requirement.NoDietary;
      plus_ones = 0;
    }
    data
  |> Result.map Core.Rsvp.rsvp |> Lwt.return

let get ?(err = None) = fun req -> Lift.html Page.Rsvp.get req err

let post =
  Lift.form ~onerr:(fun req err -> get ~err req) post_handler
  @@ Lift.html Page.Rsvp.post
