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

module Toggle = struct
  let container_cl = [ "toggle" ]
  let ctrl_cl = [ "toggle"; "ctrl" ]
  let on_cl = [ "toggle"; "on" ]
  let off_cl = [ "toggle"; "off" ]

  let ctrl ?(checked = false) () =
    a_class ctrl_cl :: (if checked then [ a_checked () ] else [])

  let make ?(a = []) content = div ~a:(a_class container_cl :: a) content

  let button ?(a = []) ?checked content =
    label
      ~a:(a_class [ "button" ] :: a)
      (input
         ~a:(a_input_type `Checkbox :: a_hidden `Hidden :: ctrl ?checked ())
         ()
      :: content)

  let on ?(a = []) content = slot ~a:(a_class on_cl :: a) content
  let off ?(a = []) content = slot ~a:(a_class off_cl :: a) content
end
