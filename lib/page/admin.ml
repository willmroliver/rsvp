open Tyxml.Html
open Core.Admin
open Common
open Domain
open User
open Html
open Css
open Components
open Forms

let user_to_row ctx user =
  let user_id = string_of_int @@ User.id_to_int user.id in
  [
    cell [ txt user_id ];
    cell [ txt user.first_name ];
    cell [ txt user.last_name ];
    cell
      [ txt @@ Option.fold ~none:"" ~some:Contact.email_to_string user.email ];
    cell
      [ txt @@ Option.fold ~none:"" ~some:Contact.mobile_to_string user.mobile ];
    cell
      ~a:[ Css.style [ ("text-align", "right") ] ]
      [
        flex ~dir:Row
          ~style:[ Css.gap @@ Css.var "gap-sm" ]
          [
            link
              ~path:(Context.path ctx [ user_id ])
              [ button [ icon "eye-outline" ] ];
            event
              ~submit:
                [
                  button
                    ~a:[ a_class [ "danger" ] ]
                    [ icon "trash-can-outline" ];
                ]
              ~action:(Context.path ctx [ user_id ])
              ~csrf:ctx.csrf
              [ ("id", user_id); ("action", "delete") ];
          ];
      ];
  ]

let list ctx { n; users } =
  let each = user_to_row ctx in
  Index.index ctx
    (table ~pin_last:true
       ~style:[ ("background", Css.alpha ~alpha:".8" "surface") ]
       ~headers:
         [ "ID"; "First Name"; "Last Name"; "Email"; "Mobile"; "Actions" ]
       ~each users)

let get ctx user =
  let open Css in
  let style =
    [
      padding @@ var "p-md";
      rounded @@ var "r-md";
      border "1px";
      ("background", alpha ~alpha:".92" "surface");
    ]
  in

  let open Ctrl in
  let user_email =
    Option.fold ~none:"" ~some:Contact.email_to_string user.email
  in
  let user_mobile =
    Option.fold ~none:"" ~some:Contact.mobile_to_string user.mobile
  in

  Editor.make
    ~toggle:[ Editor.off [ icon "pencil" ]; Editor.on [ icon "eye-outline" ] ]
    [
      grid ~cols:"auto" ~rows:"auto"
        ~style:
          ([
             gap @@ var "gap-lg";
             padding @@ var "p-lg";
             rounded @@ var "r-md";
             border "1px";
             ("box-shadow", var "elevation");
           ]
          @ glass "surface")
      @@ [
           Editor.off
           @@ col ~col:"1" ~style
                [ txt "First Name"; txt "Last Name"; txt "Email"; txt "Mobile" ]
           @ col ~col:"2" ~style
               [
                 txt user.first_name;
                 txt user.last_name;
                 txt user_email;
                 txt user_mobile;
               ];
           Editor.on
           @@ col ~col:"1"
                [
                  Forms.field
                    ~a_input:[ a_value user.first_name ]
                    ~title:"First Name" "first_name" `Text;
                  Forms.field
                    ~a_input:[ a_value user.last_name ]
                    ~title:"Last Name" "last_name" `Text;
                  Forms.field
                    ~a_input:[ a_value user_email ]
                    ~title:"Email" "email" `Email;
                  Forms.field
                    ~a_input:[ a_value user_mobile ]
                    ~title:"Mobile" "mobile" `Tel;
                ];
         ];
    ]
  |> Index.index ctx
