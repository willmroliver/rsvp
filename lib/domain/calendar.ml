type day = Day of int

let day_to_int = function Day v -> v
let day_of_int v = Day (((((v - 1) mod 31) + 31) mod 31) + 1)

type month = Month of int

let month_to_int = function Month v -> v
let month_of_int v = Month (((((v - 1) mod 12) + 12) mod 12) + 1)

type year = Year of int

let year_to_int = function Year v -> v
let year_of_int v = Year v

type date_ints = { day : int; month : int; year : int }
type date = { day : day; month : month; year : year }

let date_to_ints ({ day; month; year } : date) =
  ({ day = day_to_int day; month = month_to_int month; year = year_to_int year }
    : date_ints)

let make_date ~year ~month ~day =
  { day = day_of_int day; month = month_of_int month; year = year_of_int year }

type hour = Hour of int

let hour_to_int = function Hour v -> v
let hour_of_int v = Hour (((v mod 24) + 24) mod 24)

type minute = Minute of int

let minute_to_int = function Minute v -> v
let minute_of_int v = Minute (((v mod 60) + 60) mod 60)

type time_ints = { hour : int; minute : int }
type time = { hour : hour; minute : minute }

let time_to_ints ({ hour; minute } : time) =
  ({ hour = hour_to_int hour; minute = minute_to_int minute } : time_ints)

let make_time ~hour ~minute =
  { hour = hour_of_int hour; minute = minute_of_int minute }

type datetime = { date : date; time : time }

let make_datetime ~year ~month ~day ~hour ~minute =
  { date = make_date ~year ~month ~day; time = make_time ~hour ~minute }

let datetime_of_date ?(hour = 0) ?(minute = 0) date =
  { date; time = make_time ~hour ~minute }

type compare_result = LT | Eq | GT

let compare_result_of_int v = if v < 0 then LT else if v > 0 then GT else Eq

let compare (a : datetime) (b : datetime) =
  List.find_opt
    (fun v -> v <> 0)
    [
      compare a.date.year b.date.year;
      compare a.date.month b.date.month;
      compare a.date.day b.date.day;
      compare a.time.hour b.time.hour;
      compare a.time.minute b.time.minute;
    ]
  |> Option.value ~default:0 |> compare_result_of_int

type span = { from : datetime; until : datetime }

let make_span dt0 dt1 =
  match compare dt0 dt1 with
  | LT | Eq -> { from = dt0; until = dt1 }
  | GT -> { from = dt1; until = dt0 }
