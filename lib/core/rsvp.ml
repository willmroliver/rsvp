open Domain.Rsvp

let rsvp
    ({ status; arriving_from; returning_to; transport; dietary; plus_ones } :
      rsvp) =
  { status; arriving_from; returning_to; transport; dietary; plus_ones }
