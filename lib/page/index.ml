open Tyxml.Html
open Common.Context

let ln a rel href =
  match a with
  | Some a -> link ~rel:[ rel ] ~href ~a ()
  | None -> link ~rel:[ rel ] ~href ()

let stylesheet ?a href = ln a `Stylesheet href
let preconnect ?a href = ln a `Preconnect href
let google_font href = link ~href ~rel:[ `Stylesheet ] ()

open Html

let links =
  ([
     { title = "Home"; href = "/"; children = [] };
     { title = "RSVP"; href = "/rsvp/new"; children = [] };
     { title = "Add User"; href = "/admin/users/new"; children = [] };
     { title = "View Users"; href = "/admin/users"; children = [] };
     { title = "View RSVPs"; href = "/admin/rsvp"; children = [] };
   ]
    : Components.nav_item list)

let index ?width ctx content =
  Format.asprintf "%a" (pp ())
  @@ html
       ~a:[ a_lang "en-GB" ]
       (head
          (title (txt "RSVP"))
          [
            meta ~a:[ a_charset "utf-8" ] ();
            meta
              ~a:
                [
                  a_name "viewport";
                  a_content "width=device-width, initial-scale=1";
                ]
              ();
            meta ~a:[ a_name "color-scheme"; a_content "light dark" ] ();
            stylesheet "/static/animation.css";
            stylesheet "/static/colour.css";
            stylesheet "/static/style.css";
            stylesheet "/static/form.css";
            stylesheet "/static/table.css";
            stylesheet "/static/vars.css";
            stylesheet "/static/ctrl.css";
            stylesheet "/mdi/css/materialdesignicons.min.css";
            preconnect "https://fonts.googleapis.com";
            preconnect "https://fonts.gstatic.com"
              ~a:[ a_crossorigin `Anonymous ];
            stylesheet
              ("https://fonts.googleapis.com/css2?family=Dancing+Script:wght@400..700&"
             ^ "family=Lora:ital,wght@0,400..700;1,400..700&display=swap");
          ])
       (let cell_styles = [ Css.padding (Css.var "p-lg") ] in
        body
          [
            Ctrl.Theme.input ctx.theme ();
            Components.grid ~cols:"1fr"
              ~rows:(Css.var "nav-h" ^ " 1fr")
              [
                Components.cel ~col:"1" ~row:"1"
                  ~style:
                    ([
                       Css.border (Css.var "bw");
                       ("position", "sticky");
                       ("top", "0");
                       ("z-index", "100");
                     ]
                    @ cell_styles @ Css.glass "surface")
                  [
                    Components.navbar ~flex:Components.Row
                      ~style:
                        [
                          Css.gap (Css.var "gap-lg");
                          ("align-items", "center");
                          ("height", "100%");
                        ]
                      ~after:
                        [
                          (let v, icon =
                             match ctx.theme with
                             | Light -> ("dark", "weather-sunny")
                             | _ -> ("light", "weather-night")
                           in
                           Forms.form
                             ~a:[ a_style "margin-left: auto;" ]
                             ~submit:(Components.icon ~size:"md" icon)
                             ~csrf:ctx.csrf
                             [
                               input
                                 ~a:
                                   [
                                     a_input_type `Hidden;
                                     a_name "theme";
                                     a_value v;
                                   ]
                                 ();
                             ]);
                        ]
                      ~items:links ();
                  ];
                Components.cel ~col:"1" ~row:"2" ~style:cell_styles
                  [
                    h1
                      ~a:
                        [
                          (Css.style
                          @@ [ ("z-index", "-1"); ("text-align", "center") ]
                          @
                          if ctx.target = "/" then []
                          else [ Css.position "absolute" ]);
                        ]
                      [ txt "Will Oliver's Type-Safe Polymorphic RSVP Site" ];
                    Components.flex ~dir:Col
                      ~style:
                        [
                          ("margin", "0 auto");
                          ( "width",
                            Option.value
                              ~default:("min(100%, " ^ Css.var "content-w" ^ ")")
                              width );
                        ]
                      [ content ];
                  ];
              ];
          ])
