open Tyxml.Html

type data_attr = Html_types.nmtoken * Html_types.text

let list_opt = Option.value ~default:[]

let make_a style opt_s opt_a =
  [ Css.style @@ style @ Option.value ~default:[] opt_s ]
  @ Option.value ~default:[] opt_a

let grid ?cols ?rows ?style ?a =
  div
    ~a:
      (make_a
         ([ Css.display "grid" ]
         @ Option.fold ~none:[]
             ~some:(fun cols -> [ ("grid-template-columns", cols) ])
             cols
         @ Option.fold ~none:[]
             ~some:(fun rows -> [ ("grid-template-rows", rows) ])
             rows)
         style a)

let cel ~col ~row ?style ?a =
  div ~a:(make_a [ ("grid-column", col); ("grid-row", row) ] style a)

let col ~col ?style ?a =
  List.mapi (fun i el ->
      div
        ~a:
          (make_a
             [ ("grid-column", col); ("grid-row", string_of_int (i + 1)) ]
             style a)
        [ el ])

let row ~row ?style ?a =
  List.mapi (fun i el ->
      div
        ~a:
          (make_a
             [ ("grid-column", string_of_int (i + 1)); ("grid-row", row) ]
             style a)
        [ el ])

type flex_dir = Row | Col

let flex ~dir ?style ?a =
  div
    ~a:
      (make_a
         [
           ("display", "flex");
           ("flex-direction", match dir with Row -> "row" | Col -> "column");
         ]
         style a)

type nav_item = { title : string; href : string; children : nav_item list }

let item_to_el item = li [ a ~a:[ a_href item.href ] [ txt item.title ] ]

let navbar ~items ?style ?a ?flex ?before ?after () =
  nav
    ~a:
      (make_a
         (match flex with
         | Some dir ->
             [
               ("display", "flex");
               ( "flex-direction",
                 match dir with Row -> "row" | Col -> "column" );
             ]
         | None -> [])
         style a)
    (Option.value before ~default:[]
    @ [ ul @@ List.map item_to_el items ]
    @ Option.value after ~default:[])

let icon ?style ?a ?size name =
  i
    ~a:
      (make_a
         (Option.fold ~none:[]
            ~some:(fun size -> [ ("font-size", Css.var @@ "fs-" ^ size) ])
            size)
         style a
      @ [ a_class [ "mdi"; "mdi-" ^ name ] ])
    []
