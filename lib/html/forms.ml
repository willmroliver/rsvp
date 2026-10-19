open Tyxml.Html
open Components

let state = ref 0

let next_id name =
  incr state;
  name ^ "-" ^ string_of_int !state

let label_for a text id = label ~a:([ a_label_for id ] @ a) [ txt text ]

let field ?title ?(a = []) ?(a_label = []) ?(a_input = []) name kind =
  let id = next_id name in

  div ~a
  @@ Option.fold ~none:[]
       ~some:(fun text -> [ label_for a_label text id ])
       title
  @ [
      input
        ~a:
          ([ a_id id; a_input_type kind; a_name name; a_autocomplete `On ]
          @ a_input)
        ();
    ]

type selopt = { name : string; value : string }

let options ?title ?(a_label = []) ?(a_select = []) ?(a_option = []) name opts =
  let id = next_id name in

  div
  @@ Option.fold ~none:[]
       ~some:(fun text -> [ label_for a_label text id ])
       title
  @ [
      select ~a:([ a_id id; a_name name; a_autocomplete `On ] @ a_select)
      @@ List.map
           (fun o -> option ~a:([ a_value o.value ] @ a_option) @@ txt o.name)
           opts;
    ]

let checkbox ?(a = []) ~title name =
  let id = next_id name in
  label
    ~a:[ a_label_for id ]
    [
      input ~a:([ a_input_type `Checkbox; a_name name; a_id id ] @ a) ();
      txt title;
    ]

let radio ?title ?(a = []) ?(a_input = []) name opts =
  fieldset ~legend:(legend [ txt @@ Option.value ~default:name title ]) ~a
  @@ List.map
       (fun o ->
         let id = next_id name in
         label
           [
             input
               ~a:
                 ([ a_input_type `Radio; a_name name; a_value o.value; a_id id ]
                 @ a_input)
               ();
             txt o.name;
           ])
       opts

let form ?(a = []) ?submit ?(action = "") ?(mthd = `Post) ~csrf fields =
  form
    ~a:([ a_action action; a_method mthd ] @ a)
    ([ input ~a:[ a_name "dream.csrf"; a_input_type `Hidden; a_value csrf ] () ]
    @ fields
    @ [
        button
          ~a:[ a_button_type `Submit ]
          [ (match submit with Some el -> el | None -> txt "Submit") ];
      ])

let event ?(a = []) ?submit ?(action = "") ?(mthd = `Post) ~csrf data =
  Tyxml.Html.form
    ~a:([ a_action action; a_method mthd ] @ a)
    ([ input ~a:[ a_name "dream.csrf"; a_input_type `Hidden; a_value csrf ] () ]
    @ List.map
        (fun (k, v) ->
          input ~a:[ a_name k; a_input_type `Hidden; a_value v ] ())
        data
    @
    match submit with
    | Some el -> el
    | None -> [ button ~a:[ a_button_type `Submit ] [ txt "Submit" ] ])

let link ?a ?query ~path els =
  let query =
    match query with
    | Some query ->
        "?" ^ String.concat "," @@ List.map (fun (k, v) -> k ^ "=" ^ v) query
    | None -> ""
  in
  Tyxml.Html.a ~a:([ a_href @@ path ^ query ] @ Option.value ~default:[] a) els
