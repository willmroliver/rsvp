type prop = (string * string) list

let to_str (k, v) = k ^ ": " ^ v
let to_style props = String.concat "; " (List.map to_str props)
let append style props = style ^ "; " ^ to_style props
let style props = Tyxml.Html.a_style @@ to_style props
let var v = "var(--" ^ v ^ ")"
let margin v = ("margin", v)
let padding v = ("padding", v)
let rounded v = ("border-radius", v)
let border v = ("border", v ^ " solid " ^ var "border")
let gap v = ("gap", v)
let color v = ("color", v)
let bgcolor v = ("background-color", v)
