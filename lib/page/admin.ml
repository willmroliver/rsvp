open Tyxml.Html
open Core.Admin
open Domain
open User
open Html
open Css
open Components

let get request { n; users } =
  Index.index request
    (grid ~rows:"auto" ~cols:"auto" ~style:[ gap @@ var "g-sm" ]
    @@ col ~col:"1"
    @@ List.map (fun user -> txt user.first_name) users)
