open Proof.Strings

type transport = NoTransport | Arriving | Leaving | BothWays

let fold_transport f acc =
  List.fold_right f [ NoTransport; Arriving; Leaving; BothWays ] acc

type dietary =
  | NoDietary
  | Vegetarian
  | Vegan
  | Pescatarian
  | Allergies of string_proof alpha t list

let fold_dietary f acc =
  List.fold_right f
    [ NoDietary; Vegetarian; Vegan; Pescatarian; Allergies [] ]
    acc
