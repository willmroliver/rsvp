module Listing = struct
  type args = { off : int; lim : int }
  type 'a args_dict = { off : 'a; lim : 'a }
end

module Action = struct
  type kind = Create | Read | Update | Delete
end
