open Domain.Rsvp

let rsvp db
    ({ status; arriving_from; returning_to; transport; dietary; plus_one } :
      rsvp) =
  { status; arriving_from; returning_to; transport; dietary; plus_one }
