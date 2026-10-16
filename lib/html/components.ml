open Tyxml.Html

type data_attr = Html_types.nmtoken * Html_types.text

let opt_list = Option.value ~default:[]

let grid ?cols ?rows ?a =
  div
    ~a:
      ([
         Css.style
         @@ [ Css.display "grid" ]
         @ Option.fold ~none:[]
             ~some:(fun cols -> [ ("grid-template-columns", cols) ])
             cols
         @ Option.fold ~none:[]
             ~some:(fun rows -> [ ("grid-template-rows", rows) ])
             rows;
       ]
      @ opt_list a)

let cel ~col ~row ?a =
  div
    ~a:
      ([ Css.style @@ [ ("grid-column", col); ("grid-row", row) ] ] @ opt_list a)

let col ~col ?a =
  List.mapi (fun i el ->
      div
        ~a:
          ([
             Css.style
               [ ("grid-column", col); ("grid-row", string_of_int (i + 1)) ];
           ]
          @ opt_list a)
        [ el ])

let row ~row ?a =
  List.mapi (fun i el ->
      div
        ~a:
          ([
             Css.style
               [ ("grid-column", string_of_int (i + 1)); ("grid-row", row) ];
           ]
          @ opt_list a)
        [ el ])

type flex_dir = Row | Col

let flex ~dir ?a =
  div
    ~a:
      ([
         Css.style
           [
             ("display", "flex");
             ("flex-direction", match dir with Row -> "row" | Col -> "column");
           ];
       ]
      @ opt_list a)

type nav_item = { title : string; href : string; children : nav_item list }

let item_to_el item = li [ a ~a:[ a_href item.href ] [ txt item.title ] ]

let navbar ~items ?a ?flex ?before ?after () =
  nav
    ~a:
      ([
         Css.style
           (match flex with
           | Some dir ->
               [
                 ("display", "flex");
                 ( "flex-direction",
                   match dir with Row -> "row" | Col -> "column" );
               ]
           | None -> []);
       ]
      @ opt_list a)
    (Option.value before ~default:[]
    @ [ ul @@ List.map item_to_el items ]
    @ Option.value after ~default:[])

let icon ?a ?size name =
  i
    ~a:
      ([
         a_class [ "mdi"; "mdi-" ^ name ];
         Css.style
         @@ Option.fold ~none:[]
              ~some:(fun size -> [ ("font-size", Css.var @@ "fs-" ^ size) ])
              size;
       ]
      @ opt_list a)
    []
