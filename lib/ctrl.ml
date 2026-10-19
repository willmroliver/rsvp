open Tyxml.Html

module Theme = struct
  type theme = Light | Dark | Default

  let of_string s =
    match s with "light" -> Light | "dark" -> Dark | _ -> Default

  let to_string t =
    match t with Light -> "light" | Dark -> "dark" | Default -> "default"

  let theme_request fields =
    match fields with
    | ("theme", theme) :: [] -> Some (of_string theme)
    | _ -> None

  let from_session req =
    match Dream.session_field req "theme" with
    | Some theme -> of_string theme
    | None -> Default

  let middleware handler req =
    match%lwt Dream.form req with
    | `Ok fields -> (
        match theme_request fields with
        | None -> handler req
        | Some th ->
            let%lwt () = Dream.set_session_field req "theme" (to_string th) in
            Dream.redirect req (Dream.target req))
    | _ -> handler req

  let input theme =
    input
      ~a:
        [
          a_input_type `Checkbox;
          a_hidden `Hidden;
          a_id ("theme-" ^ to_string theme);
          a_checked ();
        ]
end

module Editor = struct
  let input_id = "edit"
  let editor_cl = "editor"
  let on_cl = "on"
  let off_cl = "off"

  let state =
    input ~a:[ a_input_type `Checkbox; a_hidden `Hidden; a_id input_id ]

  let toggle ?(a = []) content =
    label ~a:([ a_label_for input_id; a_class [ "button" ] ] @ a) content

  open Html
  open Components
  open Css

  let make ?toggle:t content =
    div ~a:[ a_class [ editor_cl ] ]
    @@ Option.fold ~none:content
         ~some:(fun t ->
           [
             flex ~dir:Col
               ~style:[ gap @@ var "gap-sm" ]
               (div ~a:[ style [ ("margin-left", "auto") ] ] [ toggle t ]
               :: content);
           ])
         t

  let on content = slot ~a:[ a_class [ editor_cl; on_cl ] ] content
  let off content = slot ~a:[ a_class [ editor_cl; off_cl ] ] content
end

module Toggle = struct
  let container = [ "toggle" ]
  let input = [ "toggle"; "ctrl" ]
  let on = [ "toggle"; "on"; "slide-in-top" ]
  let off = [ "toggle"; "off"; "slide-in-top" ]
end
