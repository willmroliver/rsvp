open Proof.Strings

type address_id = AddressID of int

let id_to_int = function AddressID id -> id

type address_line = AddressLine of string_proof non_empty t
type city = City of string_proof non_empty t
type country = Country of string_proof non_empty t
type post_code = PostCode of string_proof non_empty t

type 'a dict = {
  id : 'a;
  line_1 : 'a;
  line_2 : 'a;
  city : 'a;
  country : 'a;
  post_code : 'a;
}

type address = {
  id : address_id;
  line_1 : address_line;
  line_2 : address_line option;
  city : city option;
  country : country option;
  post_code : post_code;
}

let to_strings address =
  ({
     id = string_of_int @@ id_to_int address.id;
     line_1 = (match address.line_1 with AddressLine v -> to_string v);
     line_2 =
       (match address.line_2 with
       | Some (AddressLine v) -> to_string v
       | None -> "");
     city =
       (match address.city with Some (City v) -> to_string v | None -> "");
     country =
       (match address.country with
       | Some (Country v) -> to_string v
       | None -> "");
     post_code = (match address.post_code with PostCode v -> to_string v);
   }
    : string dict)

let from_strings
    ({ id; line_1; line_2; city; country; post_code } : string dict) =
  let to_opt = fun v -> non_empty @@ of_string v in

  Option.product (to_opt line_1) (to_opt post_code)
  |> Option.product (int_of_string_opt id)
  |> Option.map (fun (id, (line_1, post_code)) ->
      {
        id = AddressID id;
        line_1 = AddressLine line_1;
        line_2 = to_opt line_2 |> Option.map (fun v -> AddressLine v);
        city = to_opt city |> Option.map (fun v -> City v);
        country = to_opt country |> Option.map (fun v -> Country v);
        post_code = PostCode post_code;
      })

let fold f acc ({ line_1; line_2; city; country; post_code } : 'a dict) =
  List.fold_right f [ line_1; line_2; city; country; post_code ] acc

let zip (a : 'a dict) (b : 'b dict) =
  ({
     id = (a.id, b.id);
     line_1 = (a.line_1, b.line_1);
     line_2 = (a.line_2, b.line_2);
     city = (a.city, b.city);
     country = (a.country, b.country);
     post_code = (a.post_code, b.post_code);
   }
    : ('a * 'b) dict)
