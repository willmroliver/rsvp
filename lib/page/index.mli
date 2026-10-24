open Common

val index :
  ?width:string ->
  Context.t ->
  [< Html_types.div_content_fun > `H1 ] Tyxml.Html.elt ->
  Tyxml.Html.uri
