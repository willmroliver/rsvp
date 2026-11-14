open Tyxml.Html
open Common.Model.Rsvp
open Core.Admin
open Domain
open Html
open Css
open Components
open Forms

let encode = Wire.Strings.Rsvp.Model.field_encode

let rsvp_to_row full_name rsvp =
  List.map
    (function
      | Rsvp.Any f ->
          td
            [
              flex ~dir:Row
                ~style:[ ("text-wrap", "nowrap"); gap @@ var "gap-sm" ]
                ([ txt @@ rsvp_to_display rsvp f ]
                @
                if labels f = labels UserID then
                  [
                    link
                      ~path:("/admin/users/" ^ encode rsvp UserID)
                      [
                        button
                          ~a:[ a_class [ "accent" ] ]
                          [ icon ~size:"xs" "eye-outline" ];
                      ];
                  ]
                else []);
            ])
    Rsvp.all

let list ctx { n; rsvps } =
  let each =
   fun ((user : User.user), rsvp) ->
    rsvp_to_row (user.first_name ^ " " ^ user.last_name) rsvp
  in

  table ~style:(Css.glass "surface")
    ~headers:(List.map (function Rsvp.Any f -> labels f) Rsvp.all)
    ~each rsvps
  |> Index.index ~width:"100%" ctx
