let date_round_trip s =
  let open Rsvp.Wire.Strings in
  let dt = Calendar.decode s in
  Option.fold ~none:false ~some:(fun v -> s = Calendar.encode v) dt

let () =
  assert (date_round_trip "2026-11-03T13:33:00.0");
  assert (not @@ date_round_trip "2026-1-03T13:33:00.0");
  assert (date_round_trip "2026-01-03T13:33:00.0");
  assert (not @@ date_round_trip "2026-11-3T13:33:00.0");
  assert (date_round_trip "2026-11-03T13:33:00.0");
  assert (not @@ date_round_trip "2026-11-03T1:33:00.0");
  assert (date_round_trip "2026-11-03T01:33:00.0");
  assert (not @@ date_round_trip "2026-11-03T13:3:00.0");
  assert (date_round_trip "2026-11-03T13:03:00.0");
  assert (not @@ date_round_trip "2026-11-03T13:33");
  ()
