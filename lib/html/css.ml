let style props =
  Tyxml.Html.a_style @@ String.concat "; "
  @@ List.map (fun (k, v) -> k ^ ": " ^ v) props

(* spacing *)
let var v = "var(--" ^ v ^ ")"
let margin v = ("margin", v)
let padding v = ("padding", v)
let rounded v = ("border-radius", v)
let border v = ("border", v ^ " solid " ^ var "border")
let gap v = ("gap", v)

(* cosmetic *)
let color v = ("color", v)
let bgcolor v = ("background-color", v)

(* layout & position *)
let display v = ("display", v)
let position v = ("position", v)
let top v = ("top", v)
let bottom v = ("bottom", v)
let left v = ("left", v)
let right v = ("right", v)
