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

let encode = Wire.Strings.User.field_encode

let user_to_row ctx user =
  let id = encode user ID in
  let first_name = encode user FirstName in
  let last_name = encode user LastName in
  let email = encode user Email in
  let mobile = encode user Mobile in
  [
    td [ txt id ];
    td [ txt first_name ];
    td [ txt last_name ];
    td [ txt email ];
    td [ txt mobile ];
    td
      ~a:[ style [ ("text-align", "right") ] ]
      [
        flex ~dir:Row
          ~style:[ gap @@ var "gap-sm" ]
          [
            link ~path:(Context.path ctx [ id ])
              [ button [ icon "eye-outline" ] ];
            event
              ~submit:
                [
                  button
                    ~a:[ a_class [ "danger" ] ]
                    [ icon "trash-can-outline" ];
                ]
              ~action:(Context.path ctx [ id ]) ~csrf:ctx.csrf
              [ Wire.Strings.Action.codec.encode ((), Delete) ];
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

let editor ~(ctx : Context.t) ~kind ~action user =
  let first_name = encode user FirstName in
  let last_name = encode user LastName in
  let email = encode user Email in
  let mobile = encode user Mobile in

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
                   [ txt first_name; txt last_name; txt email; txt mobile ];
               Toggle.on
               @@ col ~col:"1"
                    [
                      (Forms.form ~action ~csrf:ctx.csrf ~submit:(txt "Save")
                      @@ let open Wire.Strings.User in
                         let field f t k =
                           (encode user f, field_name f, t, k)
                         in
                         (List.map (fun (f, v, t, k) ->
                              Forms.field ~a_input:[ a_value v ] ~title:t f k)
                         @@ List.filter_map
                              (function
                                | Any FirstName ->
                                    Some (field FirstName "First Name" `Text)
                                | Any LastName ->
                                    Some (field LastName "Last Name" `Text)
                                | Any Email -> Some (field Email "Email" `Email)
                                | Any Mobile ->
                                    Some (field Mobile "Mobile" `Tel)
                                | _ -> None)
                              all)
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
