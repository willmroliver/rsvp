open Tyxml.Html
open Common.Model.Rsvp
open Core.Admin
open Domain
open Html
open Css
open Components
open Forms

let rsvp_to_row full_name (rsvp : (string * string) Rsvp.dict) =
  Rsvp.fold
    (fun (l, v) acc ->
      td
        [
          flex ~dir:Row
            ~style:[ ("text-wrap", "nowrap"); gap @@ var "gap-sm" ]
            ([ txt v ]
            @
            if l = labels.user_id then
              [
                link
                  ~path:("/admin/users/" ^ snd rsvp.user_id)
                  [
                    button
                      ~a:[ a_class [ "accent" ] ]
                      [ icon ~size:"xs" "eye-outline" ];
                  ];
              ]
            else []);
        ]
      :: acc)
    []
    { rsvp with user_id = (fst rsvp.user_id, full_name) }

let list ctx { n; rsvps } =
  let each =
   fun ((user : User.user), rsvp) ->
    rsvp_to_display rsvp |> Rsvp.zip labels
    |> rsvp_to_row (user.first_name ^ " " ^ user.last_name)
  in

  table ~style:(Css.glass "surface")
    ~headers:(Rsvp.fold (fun v acc -> v :: acc) [] labels)
    ~each rsvps
  |> Index.index ~width:"100%" ctx
