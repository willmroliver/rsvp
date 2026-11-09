open Rsvp.Domain.Calendar

let test_compare () =
  let date = make_datetime ~year:2026 ~month:11 ~day:6 ~hour:19 ~minute:57 in
  let next_minute =
    make_datetime ~year:2026 ~month:11 ~day:6 ~hour:19 ~minute:58
  in
  let next_hour =
    make_datetime ~year:2026 ~month:11 ~day:7 ~hour:20 ~minute:57
  in
  let next_day =
    make_datetime ~year:2026 ~month:11 ~day:7 ~hour:19 ~minute:57
  in
  let next_month =
    make_datetime ~year:2026 ~month:12 ~day:6 ~hour:19 ~minute:57
  in
  let next_year =
    make_datetime ~year:2027 ~month:11 ~day:6 ~hour:19 ~minute:57
  in

  assert (compare date date = Eq);
  assert (compare date next_minute = LT);
  assert (compare next_minute date = GT);
  assert (compare date next_hour = LT);
  assert (compare next_hour date = GT);
  assert (compare date next_day = LT);
  assert (compare next_day date = GT);
  assert (compare date next_month = LT);
  assert (compare next_month date = GT);
  assert (compare date next_year = LT);
  assert (compare next_year date = GT);

  let only_year_gt =
    make_datetime ~year:2027 ~month:10 ~day:5 ~hour:18 ~minute:56
  in

  assert (compare date only_year_gt = LT);
  assert (compare only_year_gt date = GT);
  ()

let test_span () =
  let date = make_date ~year:2027 ~month:11 ~day:6 in
  let one = datetime_of_date ~hour:20 ~minute:18 date in
  let two = datetime_of_date ~hour:20 ~minute:19 date in
  let s = make_span one two in
  let t = make_span two one in
  assert (s = t);
  assert (s.from = one);
  assert (t.until = two);
  ()

let () =
  test_compare ();
  test_span ();
  ()
