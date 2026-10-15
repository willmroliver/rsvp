open Tyxml.Html

type data_attr = Html_types.nmtoken * Html_types.text

let cmp_attrs style cl a =
  [ a_style @@ Css.to_style style ]
  @ Option.fold ~none:[] ~some:(fun cl -> [ a_class cl ]) cl
  @ Option.value a ~default:[]

let grid ?cols ?rows ?style ?cl ?a els =
  div
    ~a:
      (cmp_attrs
         ([ ("display", "grid") ]
         @ Option.fold ~none:[]
             ~some:(fun cols -> [ ("grid-template-columns", cols) ])
             cols
         @ Option.fold ~none:[]
             ~some:(fun rows -> [ ("grid-template-rows", rows) ])
             rows
         @ Option.value style ~default:[])
         cl a)
    els

let cel ~col ~row ?style ?cl ?a els =
  div
    ~a:
      (cmp_attrs
         ([ ("grid-column", col); ("grid-row", row) ]
         @ Option.value style ~default:[])
         cl a)
    els

let col ~col ?style ?cl ?a els =
  List.mapi
    (fun i el ->
      div
        ~a:
          (cmp_attrs
             ([ ("grid-column", col); ("grid-row", string_of_int (i + 1)) ]
             @ Option.value style ~default:[])
             cl a)
        [ el ])
    els

let row ~row ?style ?cl ?a els =
  List.mapi
    (fun i el ->
      div
        ~a:
          (cmp_attrs
             ([ ("grid-column", string_of_int (i + 1)); ("grid-row", row) ]
             @ Option.value style ~default:[])
             cl a)
        [ el ])
    els

type flex_dir = Row | Col

let flex ~dir ?style ?cl ?a els =
  div
    ~a:
      (cmp_attrs
         ([
            ("display", "flex");
            ("flex-direction", match dir with Row -> "row" | Col -> "column");
          ]
         @ Option.value style ~default:[])
         cl a)
    els

type nav_item = { title : string; href : string; children : nav_item list }

let item_to_el item = li [ a ~a:[ a_href item.href ] [ txt item.title ] ]

let navbar ~items ?style ?cl ?a ?flex ?before ?after () =
  nav
    ~a:
      (cmp_attrs
         ((match flex with
            | Some dir ->
                [
                  ("display", "flex");
                  ( "flex-direction",
                    match dir with Row -> "row" | Col -> "column" );
                ]
            | None -> [])
         @ Option.value style ~default:[])
         cl a)
    (Option.value before ~default:[]
    @ [ ul @@ List.map item_to_el items ]
    @ Option.value after ~default:[])

let icon ?style ?cl ?a ?size name =
  i
    ~a:
      ([ a_class [ "mdi"; "mdi-" ^ name ] ]
      @ cmp_attrs
          (Option.value style ~default:[]
          @ Option.fold ~none:[]
              ~some:(fun size -> [ ("font-size", Css.var ("fs-" ^ size)) ])
              size)
          cl a)
    []
