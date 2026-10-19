open Core.Rsvp
open Domain
open Wire.Strings.Error
module Wire = Wire.Strings.Rsvp

let fields =
  Rsvp.zip
    {
      status = "Are you free?";
      arriving_from = "Arriving from";
      returning_to = "Returning to";
      transport = "Transport help needed?";
      dietary = "Dietary requirements";
      plus_one = "Plus one?";
    }
    Wire.fields

let status_opts =
  Rsvp.fold_status
    (fun s opts ->
      let v = Wire.status_encode s in
      match s with
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
    status = (`Select, status_opts, "1", "1/3");
    arriving_from = (`Text, [], "2", "1");
    returning_to = (`Text, [], "2", "2");
    transport = (`Select, transport_opts, "3", "1/3");
    dietary = (`Select, dietary_opts, "4", "1/3");
    plus_one = (`Checkbox, [], "5", "1/3");
  }

let city_to_display =
  Option.fold ~none:"" ~some:(fun c ->
      match c with Location.City c -> Proof.Strings.to_string c)

let status_to_display = function
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
open Ctrl

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

let with_err name (err : decode_error option) =
  match err with
  | Some (BadField (field, _)) when field = name ->
      [ ("box-shadow", "0 0 2px 1px " ^ var "red"); color @@ var "red" ]
  | _ -> []

let get ctx (err : decode_error option) =
  Index.index ctx
  @@ Forms.form ~csrf:ctx.csrf
       ~a:
         [
           style @@ glass "surface"
           @ [
               padding @@ var "p-lg";
               rounded @@ var "r-md";
               ("box-shadow", var "elevation");
               border "1px";
             ];
         ]
  @@ [
       (let els =
          Rsvp.fold
            (fun ((label, name), (kind, opts, row, col)) acc ->
              (cel ~row ~col
              @@ [
                   (match kind with
                   | `Select ->
                       Forms.radio ~title:label
                         ~a:[ style @@ with_err name err ]
                         name
                         (List.map
                            (fun (name, value) ->
                              ({ name; value } : Forms.selopt))
                            opts)
                   | (`Text | `Number) as kind ->
                       Forms.field ~title:label
                         ~a_input:
                           [
                             (a_placeholder
                             @@
                             match name with
                             | name
                               when name = Wire.fields.arriving_from
                                    || name = Wire.fields.returning_to ->
                                 "E.g. \"London\", \"Milano\""
                             | _ -> "");
                             style @@ with_err name err;
                           ]
                         name kind
                   | `Checkbox ->
                       flex ~dir:Row
                         ~a:[ a_class Toggle.container ]
                         ~style:
                           [
                             ("align-items", "end"); Css.gap @@ Css.var "gap-sm";
                           ]
                         [
                           Forms.checkbox
                             ~a:
                               [
                                 a_class Toggle.input;
                                 Css.style [ ("flex-grow", "1") ];
                               ]
                             ~title:label "";
                           Forms.field
                             ~a:
                               [
                                 a_class Toggle.on;
                                 Css.style [ ("flex-grow", "1") ];
                               ]
                             ~a_input:
                               [
                                 a_placeholder "Tell us who you'd like to bring";
                               ]
                             name `Text;
                         ]);
                 ])
              :: acc)
            [] (Rsvp.zip fields inputs)
        in
        els |> grid ~cols:"auto" ~style:[ gap @@ var "gap-xl" ]);
     ]

let post ctx
    ({ status; arriving_from; returning_to; transport; dietary; plus_one } :
      Rsvp.rsvp) =
  Index.index ctx
    (let fields =
       [
         ("Status", status_to_display status);
         ("Arriving From", city_to_display arriving_from);
         ("Returning To", city_to_display returning_to);
         ("Transport", transport_to_display transport);
         ("Dietary Requirements", dietary_to_display dietary);
       ]
     in

     grid ~cols:"auto auto" ~style:[ gap @@ var "gap-lg" ]
     @@ List.flatten
     @@ List.mapi
          (fun i (k, v) -> row ~row:(string_of_int (i + 1)) [ pill k; pill v ])
          fields)
