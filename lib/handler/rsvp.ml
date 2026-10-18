open Common
open Domain

let fields = Rsvp.map (fun (_, name) -> name) Page.Rsvp.fields

let post_request =
  Wire.Strings.Rsvp.rsvp_decode
    {
      status = NotResponded;
      arriving_from = None;
      returning_to = None;
      transport = Requirement.NoTransport;
      dietary = Requirement.NoDietary;
      plus_ones = 0;
    }

let post_handler db data = Core.Rsvp.rsvp db data |> Lwt.return_ok
let get ?(err = None) = fun req -> Out.html Page.Rsvp.get req err

let post =
  In.form ~onerr:(fun req err -> get ~err req) post_request post_handler
  @@ Out.html Page.Rsvp.post
