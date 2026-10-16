open Tyxml.Html
open Html
open Html.Components

let get request () =
  Index.index request
    (let field =
       Forms.field
         ~a_label:[ Css.style @@ [ Css.padding "0" ] ]
         ~a_input:
           [
             Css.style
             @@ [
                  Css.padding (Css.var "p-lg");
                  Css.rounded (Css.var "r-sm");
                  Css.border (Css.var "bw");
                ];
             a_class [ "hover" ];
           ]
     in
     flex ~dir:Col
       [
         Forms.form ~action:"/login" ~mthd:`Post request
           [
             grid ~cols:"auto"
               ~a:[ Css.style [ ("gap", Css.var "gap-lg") ] ]
               [
                 cel ~col:"1" ~row:"1"
                   [ field ~title:"First Name" "first_name" `Text ];
                 cel ~col:"2" ~row:"1"
                   [ field ~title:"Last Name" "last_name" `Text ];
                 cel ~col:"1 / 3" ~row:"2"
                   [ field ~title:"Email" "email" `Text ];
               ];
           ];
       ])
