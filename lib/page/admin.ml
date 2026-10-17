open Tyxml.Html
open Core.Admin
open Domain
open User
open Html
open Css
open Components
open Forms

let user_to_row req user =
  let user_id = string_of_int @@ User.id_to_int user.id in
  [
    td [ txt user_id ];
    td [ txt user.first_name ];
    td [ txt user.last_name ];
    td [ txt @@ Option.fold ~none:"" ~some:Contact.email_to_string user.email ];
    td
      [ txt @@ Option.fold ~none:"" ~some:Contact.mobile_to_string user.mobile ];
    td
      ~a:[ Css.style [ ("text-align", "right") ] ]
      [
        flex ~dir:Row
          ~style:[ Css.gap @@ Css.var "gap-sm" ]
          [
            event
              ~submit:[ button [ icon "eye-outline" ] ]
              ~action:(Dream.target req ^ "/" ^ user_id)
              req [];
            event
              ~submit:
                [
                  button
                    ~a:[ a_class [ "danger" ] ]
                    [ icon "trash-can-outline" ];
                ]
              ~action:(Dream.target req ^ "/" ^ user_id)
              req
              [ ("id", user_id); ("action", "delete") ];
          ];
      ];
  ]

let get request { n; users } =
  let each = user_to_row request in
  Index.index request
    (table
       ~headers:
         [ "ID"; "First Name"; "Last Name"; "Email"; "Mobile"; "Actions" ]
       ~each users)
