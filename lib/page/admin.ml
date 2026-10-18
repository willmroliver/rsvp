open Tyxml.Html
open Core.Admin
open Core.Common.Action
open Common
open Domain
open User
open Html
open Css
open Components
open Forms

let user_to_row ctx (user : string User.dict) =
  [
    cell [ txt user.id ];
    cell [ txt user.first_name ];
    cell [ txt user.last_name ];
    cell [ txt user.email ];
    cell [ txt user.mobile ];
    cell
      ~a:[ Css.style [ ("text-align", "right") ] ]
      [
        flex ~dir:Row
          ~style:[ Css.gap @@ Css.var "gap-sm" ]
          [
            link
              ~path:(Context.path ctx [ user.id ])
              [ button [ icon "eye-outline" ] ];
            event
              ~submit:
                [
                  button
                    ~a:[ a_class [ "danger" ] ]
                    [ icon "trash-can-outline" ];
                ]
              ~action:(Context.path ctx [ user.id ])
              ~csrf:ctx.csrf
              [ (Wire.Strings.User.field.id, user.id); ("action", "delete") ];
          ];
      ];
  ]

let list ctx { n; users } =
  let each =
   fun user ->
    Wire.Strings.User.encode user
    |> User.map (fun (_, v) -> v)
    |> user_to_row ctx
  in
  Index.index ctx
    (table ~pin_last:true
       ~style:[ ("background", Css.alpha ~alpha:".8" "surface") ]
       ~headers:
         [ "ID"; "First Name"; "Last Name"; "Email"; "Mobile"; "Actions" ]
       ~each users)

let get ctx user =
  let user = Wire.Strings.User.encode user in

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
                 txt @@ snd user.first_name;
                 txt @@ snd user.last_name;
                 txt @@ snd user.email;
                 txt @@ snd user.mobile;
               ];
           Editor.on
           @@ col ~col:"1"
                [
                  (Forms.form ~csrf:ctx.csrf ~submit:(txt "Save")
                  @@ let open Wire.Strings.User in
                     List.map (fun (f, v, t, k) ->
                         Forms.field ~a_input:[ a_value v ] ~title:t f k)
                     @@ User.fold
                          (fun u l ->
                            match u with
                            | f, v when f = field.first_name ->
                                (f, v, "First Name", `Text) :: l
                            | f, v when f = field.last_name ->
                                (f, v, "Last Name", `Text) :: l
                            | f, v when f = field.email ->
                                (f, v, "Email", `Email) :: l
                            | f, v when f = field.mobile ->
                                (f, v, "Mobile", `Tel) :: l
                            | _ -> l)
                          [] user);
                ];
         ];
    ]
  |> Index.index ctx
