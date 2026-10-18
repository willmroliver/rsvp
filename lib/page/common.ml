open Tyxml.Html
open Wire.Strings

module Context = struct
  type t = { target : string; csrf : string; theme : Ctrl.Theme.theme }

  let path ?target ctx parts =
    String.concat "/" (Option.value ~default:ctx.target target :: parts)
end

module Action = struct
  let field ~kind () =
    input
      ~a:
        [
          a_input_type `Hidden;
          a_name Action.field;
          a_value @@ Action.kind_encode kind;
        ]
      ()
end
