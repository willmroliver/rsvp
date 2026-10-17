open Tyxml.Html

let state = ref 0

let label_for a text id =
  label
    ~a:([ a_label_for id ] @ Option.value ~default:[] a)
    [ txt @@ String.uppercase_ascii text ]

let field ?title ?a_label ?a_input name kind =
  incr state;
  let id = name ^ "-" ^ string_of_int !state in

  div
  @@ Option.fold ~none:[]
       ~some:(fun text -> [ label_for a_label text id ])
       title
  @ [
      input
        ~a:
          ([ a_id id; a_input_type kind; a_name name; a_autocomplete `On ]
          @ Option.value ~default:[] a_input)
        ();
    ]

type selopt = { name : string; value : string }

let options ?title ?a_label ?a_select ?a_option name opts =
  incr state;
  let id = name ^ "-" ^ string_of_int !state in

  div
  @@ Option.fold ~none:[]
       ~some:(fun text -> [ label_for a_label text id ])
       title
  @ [
      select
        ~a:
          ([ a_id id; a_name name; a_autocomplete `On ]
          @ Option.value ~default:[] a_select)
      @@ List.map
           (fun o ->
             option ~a:([ a_value o.value ] @ Option.value ~default:[] a_option)
             @@ txt o.name)
           opts;
    ]

let form ?a ?submit ?(action = "") ?(mthd = `Post) req fields =
  let csrf = Dream.csrf_token ~valid_for:86400. req in
  form
    ~a:
      ([ a_action action; a_method mthd ]
      @ match a with Some a -> a | None -> [])
    ([ input ~a:[ a_name "dream.csrf"; a_input_type `Hidden; a_value csrf ] () ]
    @ fields
    @
    match submit with
    | Some el -> el
    | None -> [ button ~a:[ a_button_type `Submit ] [ txt "SUBMIT" ] ])

let event ?a ?submit ?(action = "") ?(mthd = `Post) req data =
  let csrf = Dream.csrf_token ~valid_for:86400. req in
  Tyxml.Html.form
    ~a:
      ([ a_action action; a_method mthd ]
      @ match a with Some a -> a | None -> [])
    ([ input ~a:[ a_name "dream.csrf"; a_input_type `Hidden; a_value csrf ] () ]
    @ List.map
        (fun (k, v) ->
          input ~a:[ a_name k; a_input_type `Hidden; a_value v ] ())
        data
    @
    match submit with
    | Some el -> el
    | None -> [ button ~a:[ a_button_type `Submit ] [ txt "SUBMIT" ] ])
