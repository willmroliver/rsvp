open Tyxml.Html

let ln a rel href =
  match a with
  | Some a -> link ~rel:[ rel ] ~href ~a ()
  | None -> link ~rel:[ rel ] ~href ()

let stylesheet ?a href = ln a `Stylesheet href
let preconnect ?a href = ln a `Preconnect href
let google_font href = link ~href ~rel:[ `Stylesheet ] ()

let page request content =
  let open Tyxml.Html in
  let open Components in
  html
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
         stylesheet "/static/vars.css";
         stylesheet "/mdi/css/materialdesignicons.min.css";
         preconnect "https://fonts.googleapis.com";
         preconnect "https://fonts.gstatic.com" ~a:[ a_crossorigin `Anonymous ];
         stylesheet
           ("https://fonts.googleapis.com/css2?family=Dancing+Script:wght@400..700&"
          ^ "family=Lora:ital,wght@0,400..700;1,400..700&display=swap");
       ])
    (let theme = Theme.from_session request in
     let cell_styles = [ Css.padding (Css.var "p-lg") ] in
     body
       [
         input
           ~a:
             [
               a_input_type `Checkbox;
               a_hidden `Hidden;
               a_id ("theme-" ^ Theme.theme_to_str theme);
               a_checked ();
             ]
           ();
         grid ~cols:"1fr"
           ~rows:(Css.var "nav-h" ^ " 1fr")
           [
             cel ~col:"1" ~row:"1"
               ~style:
                 ([
                    Css.border (Css.var "bw");
                    ( "background",
                      "rgb(from " ^ Css.var "surface" ^ "r g b / 20%)" );
                    ("backdrop-filter", "blur(4px)");
                    ("position", "sticky");
                    ("top", "0");
                  ]
                 @ cell_styles)
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
                          match theme with
                          | Light -> ("dark", "weather-sunny")
                          | _ -> ("light", "weather-night")
                        in
                        Forms.form
                          ~a:[ a_style "margin-left: auto;" ]
                          ~submit:
                            [
                              button
                                ~a:[ a_button_type `Submit ]
                                [ Components.icon ~size:"md" icon ];
                            ]
                          request
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
                   ~items:Nav.main ();
               ];
             cel ~col:"1" ~row:"2" ~style:cell_styles
               [
                 flex ~dir:Col
                   ~style:
                     [
                       ("margin", "0 auto");
                       ("width", "min(100%, " ^ Css.var "content-w" ^ ")");
                     ]
                   [
                     h1 [ txt "Will Oliver's Type-Safe Polymorphic RSVP Site" ];
                     content;
                   ];
               ];
           ];
       ])

let html_to_str html = Format.asprintf "%a" (Tyxml.Html.pp ()) html
let index request content = html_to_str @@ page request content
