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
    td [ txt user.id ];
    td [ txt user.first_name ];
    td [ txt user.last_name ];
    td [ txt user.email ];
    td [ txt user.mobile ];
    td
      ~a:[ style [ ("text-align", "right") ] ]
      [
        flex ~dir:Row
          ~style:[ gap @@ var "gap-sm" ]
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
            @@ Wire.Strings.Action.encode Delete;
          ];
      ];
  ]

let list ctx { n; users } =
  let each =
   fun user -> Wire.Strings.User.encode user |> User.map snd |> user_to_row ctx
  in
  Index.index ctx
    (table ~pin_last:true
       ~style:[ ("background", Css.alpha ~alpha:".8" "surface") ]
       ~headers:
         [ "ID"; "First Name"; "Last Name"; "Email"; "Mobile"; "Actions" ]
       ~each users)

let editor ~(ctx : Context.t) ~kind ~action user =
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
  Toggle.make
    [
      flex ~dir:Col
        ~style:[ gap @@ var "gap-sm" ]
        [
          div
            ~a:[ Css.style [ ("margin-left", "auto") ] ]
            [
              Toggle.button ~checked:(kind = Create)
                [
                  Toggle.off [ icon ~size:"md" "pencil" ];
                  Toggle.on [ icon ~size:"md" "eye-outline" ];
                ];
            ];
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
               Toggle.off
               @@ col ~col:"1" ~style
                    [
                      txt "First Name";
                      txt "Last Name";
                      txt "Email";
                      txt "Mobile";
                    ]
               @ col ~col:"2" ~style
                   [
                     txt @@ snd user.first_name;
                     txt @@ snd user.last_name;
                     txt @@ snd user.email;
                     txt @@ snd user.mobile;
                   ];
               Toggle.on
               @@ col ~col:"1"
                    [
                      (Forms.form ~action ~csrf:ctx.csrf ~submit:(txt "Save")
                      @@ let open Wire.Strings.User in
                         (List.map (fun (f, v, t, k) ->
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
                              [] user)
                         @ [ Action.field ~kind () ]
                         @
                         match kind with
                         | Create ->
                             [
                               Forms.field ~a_input:[ a_value "0" ] "id" `Hidden;
                             ]
                         | _ -> []);
                    ];
             ];
        ];
    ]

let edit ctx user = editor ~ctx ~kind:Update ~action:"" user |> Index.index ctx

let create ctx () =
  editor ~ctx ~kind:Create ~action:"/admin/users"
    {
      id = User.UserID 0;
      first_name = "";
      last_name = "";
      email = None;
      mobile = None;
      address_id = None;
    }
  |> Index.index ctx
