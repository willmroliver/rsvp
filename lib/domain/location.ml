open Proof.Strings

type address_line = AddressLine of string_proof non_empty t
type city = City of string_proof non_empty t
type country = Country of string_proof non_empty t
type post_code = PostCode of string_proof non_empty t

type address = {
  line_1 : address_line;
  line_2 : address_line;
  city : city;
  country : country;
  postcode : post_code;
}
