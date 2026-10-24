open Tyxml.Html
open Wire.Strings

let field ~kind () =
  input
    ~a:
      [
        a_input_type `Hidden;
        a_name Action.field;
        a_value @@ Action.kind_encode kind;
      ]
    ()
