module Listing = struct
  type args = { off : int; lim : int }
  type 'a args_dict = { off : 'a; lim : 'a }
end

module Action = struct
  type kind = Create | Read | Update | Delete
  type 'a dict = { create : 'a; read : 'a; update : 'a; delete : 'a }

  let index ~kind (d : 'a dict) =
    match kind with
    | Create -> d.create
    | Read -> d.read
    | Update -> d.update
    | Delete -> d.delete
end
