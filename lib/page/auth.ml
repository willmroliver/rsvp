open Tyxml.Html
open Core.Auth
open Html
open Html.Components
open Html.Css

let post request { first_name; last_name; email } =
  Index.index request
    (let pill text =
       div
         ~a:
           [
             style
               [
                 rounded (var "r-md");
                 border (var "bw");
                 ("background-color", var "bg");
                 padding (var "p-sm");
               ];
           ]
         [ txt text ]
     in

     grid ~cols:"auto auto"
       ~a:[ Css.style [ gap (var "gap-lg") ] ]
       [
         cel ~col:"1" ~row:"1" [ pill first_name ];
         cel ~col:"2" ~row:"1" [ pill last_name ];
         cel ~col:"1 / 3" ~row:"2" [ pill email ];
       ])
