type day = private Day of int

val day_to_int : day -> int
val day_of_int : int -> day

type month = private Month of int

val month_to_int : month -> int
val month_of_int : int -> month

type year = private Year of int

val year_to_int : year -> int
val year_of_int : int -> year

type date_ints = private { day : int; month : int; year : int }
type date = private { day : day; month : month; year : year }

val date_to_ints : date -> date_ints
val make_date : year:int -> month:int -> day:int -> date

type hour = private Hour of int

val hour_to_int : hour -> int
val hour_of_int : int -> hour

type minute = private Minute of int

val minute_to_int : minute -> int
val minute_of_int : int -> minute

type time_ints = private { hour : int; minute : int }
type time = private { hour : hour; minute : minute }

val time_to_ints : time -> time_ints
val make_time : hour:int -> minute:int -> time

type datetime = private { date : date; time : time }

val make_datetime :
  year:int -> month:int -> day:int -> hour:int -> minute:int -> datetime

val datetime_of_date : ?hour:int -> ?minute:int -> date -> datetime

type compare_result = private LT | Eq | GT

val compare_result_of_int : int -> compare_result
val compare : datetime -> datetime -> compare_result

type span = private { from : datetime; until : datetime }

val make_span : datetime -> datetime -> span
