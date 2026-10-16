open Core.Rsvp
open Domain
module Wire = Wire.Form.Rsvp

let fields =
  Rsvp.zip
    {
      status = "Are you free?";
      arriving_from = "Arriving from";
      returning_to = "Returning to";
      transport = "Transport needed?";
      dietary = "Dietary requirements";
      plus_ones = "Plus ones?";
    }
    Wire.fields

let status_opts =
  Rsvp.fold_status
    (fun s opts ->
      let v = Wire.status_encode s in
      match s with
      | Rsvp.NotResponded -> ("", v) :: opts
      | Rsvp.Coming -> ("Coming!", v) :: opts
      | Rsvp.MaybeComing -> ("Might make it", v) :: opts
      | Rsvp.NotComing -> ("Won't be there :(", v) :: opts)
    []

let transport_opts =
  Requirement.fold_transport
    (fun t opts ->
      let v = Wire.transport_encode t in
      match t with
      | Requirement.NoTransport -> ("None", v) :: opts
      | Requirement.Arriving -> ("Arriving", v) :: opts
      | Requirement.Leaving -> ("Leaving", v) :: opts
      | Requirement.BothWays -> ("Both", v) :: opts)
    []

let dietary_opts =
  Requirement.fold_dietary
    (fun d opts ->
      let v = Wire.dietary_encode d in
      match d with
      | Requirement.NoDietary -> ("None", v) :: opts
      | Requirement.Vegetarian -> ("Vegetarian", v) :: opts
      | Requirement.Vegan -> ("Vegan", v) :: opts
      | Requirement.Pescatarian -> ("Pescatarian", v) :: opts
      | Requirement.Allergies _ -> ("Allergies", v) :: opts)
    []

let inputs : 'a Rsvp.dict =
  {
    status = (`Select, status_opts);
    arriving_from = (`Text, []);
    returning_to = (`Text, []);
    transport = (`Select, transport_opts);
    dietary = (`Select, dietary_opts);
    plus_ones = (`Number, []);
  }

let city_to_display =
  Option.fold ~none:"" ~some:(fun c ->
      match c with Location.City c -> Proof.Strings.to_string c)

let status_to_display = function
  | Rsvp.NotResponded -> "???"
  | Rsvp.Coming -> "Definitely Coming"
  | Rsvp.MaybeComing -> "Not Sure"
  | Rsvp.NotComing -> "Can't make it"

let transport_to_display = function
  | Requirement.NoTransport -> "None"
  | Requirement.Arriving -> "Arriving"
  | Requirement.Leaving -> "Leaving"
  | Requirement.BothWays -> "Both"

let dietary_to_display = function
  | Requirement.NoDietary -> "None"
  | Requirement.Vegetarian -> "Vegetarian"
  | Requirement.Pescatarian -> "Pescatarian"
  | Requirement.Vegan -> "Vegan"
  | Requirement.Allergies l ->
      "Allergies: " ^ String.concat ", " (List.map Proof.Strings.to_string l)

open Tyxml.Html
open Html.Components
open Html.Css
open Html

let pill text =
  div
    ~a:
      [
        style
          [
            rounded @@ var "r-md";
            border @@ var "bw";
            padding @@ var "p-sm";
            ("background-color", var "bg");
          ];
      ]
    [ txt text ]

let with_err name (err : Wire.decode_error option) =
  match err with
  | Some (Wire.BadField (field, _)) when String.equal field name ->
      [ ("box-shadow", "0 0 2px 1px " ^ var "red"); color @@ var "red" ]
  | _ -> []

let get request (err : Wire.decode_error option) =
  Index.index request @@ Forms.form request
  @@ [
       (let els =
          Rsvp.fold
            (fun ((label, name), (kind, opts)) acc ->
              match kind with
              | `Select ->
                  Forms.options ~title:label
                    ~a_select:
                      [ a_class [ "hover" ]; style @@ with_err name err ]
                    name
                    (List.map
                       (fun (name, value) -> ({ name; value } : Forms.selopt))
                       opts)
                  :: acc
              | (`Text | `Number) as kind ->
                  Forms.field ~title:label
                    ~a_input:
                      [
                        a_class [ "hover" ];
                        a_placeholder "...";
                        style @@ with_err name err;
                      ]
                    name kind
                  :: acc)
            [] (Rsvp.zip fields inputs)
        in
        els |> col ~col:"1"
        |> grid ~cols:"auto" ~a:[ Css.style [ gap @@ var "gap-lg" ] ]);
     ]

let post request
    ({ status; arriving_from; returning_to; transport; dietary; plus_ones } :
      Rsvp.rsvp) =
  Index.index request
    (let fields =
       [
         ("Status", status_to_display status);
         ("Arriving From", city_to_display arriving_from);
         ("Returning To", city_to_display returning_to);
         ("Transport", transport_to_display transport);
         ("Dietary Requirements", dietary_to_display dietary);
       ]
     in

     grid ~cols:"auto auto" ~a:[ Css.style [ gap @@ var "gap-lg" ] ]
     @@ List.flatten
     @@ List.mapi
          (fun i (k, v) -> row ~row:(string_of_int (i + 1)) [ pill k; pill v ])
          fields)
