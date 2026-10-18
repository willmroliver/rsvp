open Tyxml.Html
open Html
open Html.Components

let get ctx () =
  Index.index ctx
    (let field = Forms.field in
     flex ~dir:Col
       [
         Forms.form ~action:"/login" ~mthd:`Post ~csrf:ctx.csrf
           [
             grid ~cols:"auto"
               ~style:[ ("gap", Css.var "gap-lg") ]
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
