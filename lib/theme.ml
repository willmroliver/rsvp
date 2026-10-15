type theme = Light | Dark | Default

let str_to_theme s =
  match s with "light" -> Light | "dark" -> Dark | _ -> Default

let theme_to_str t =
  match t with Light -> "light" | Dark -> "dark" | Default -> "default"

let theme_request fields =
  match fields with
  | ("theme", theme) :: [] -> Some (str_to_theme theme)
  | _ -> None

let from_session req =
  match Dream.session_field req "theme" with
  | Some theme -> str_to_theme theme
  | None -> Default

let middleware handler req =
  match%lwt Dream.form req with
  | `Ok fields -> (
      match theme_request fields with
      | None -> handler req
      | Some th ->
          let%lwt () = Dream.set_session_field req "theme" (theme_to_str th) in
          Dream.redirect req (Dream.target req))
  | _ -> handler req
