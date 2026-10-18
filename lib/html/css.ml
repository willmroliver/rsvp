let style props =
  Tyxml.Html.a_style @@ String.concat "; "
  @@ List.map (fun (k, v) -> k ^ ": " ^ v) props

(* spacing *)
let var v = "var(--" ^ v ^ ")"
let margin v = ("margin", v)
let padding v = ("padding", v)
let rounded v = ("border-radius", v)
let gap v = ("gap", v)

(* colour *)
let color v = ("color", v)
let bgcolor v = ("background-color", v)
let border v = ("border", v ^ " solid " ^ var "border")

let alpha ?(alpha = ".5") hex_var =
  "rgb(from " ^ var hex_var ^ "r g b / " ^ alpha ^ ")"

let glass ?(blur = "4px") ?(opacity = ".2") hex_var =
  [
    ("background", alpha ~alpha:opacity hex_var);
    ("backdrop-filter", "blur(" ^ blur ^ ")");
  ]

let shadow ~x ~y ~blur ~spread ?(opacity = "1") hex_var =
  ( "box-shadow",
    String.concat " " [ x; y; blur; spread; alpha ~alpha:opacity hex_var ] )

(* layout & position *)
let display v = ("display", v)
let position v = ("position", v)
let top v = ("top", v)
let bottom v = ("bottom", v)
let left v = ("left", v)
let right v = ("right", v)
