open Wire.Strings

module Context = struct
  type t = { target : string; csrf : string; theme : Ctrl.Theme.theme }

  let path ?target ctx parts =
    String.concat "/" (Option.value ~default:ctx.target target :: parts)

  let action ~kind data = (Action.field, Action.kind_encode kind) :: data
end
