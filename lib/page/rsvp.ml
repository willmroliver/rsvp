open Core.Rsvp
open Domain
open Wire.Strings.Common
open Common.Model.Rsvp

let inputs : 'a Rsvp.dict =
  {
    user_id = (`Hidden, [], "", "");
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

let with_err name (res : (Rsvp.rsvp option, Error.t option) result) =
  match res with
  | Error (Some (Error.BadField (field, _))) when field = name ->
      [ ("box-shadow", "0 0 2px 1px " ^ var "red"); color @@ var "red" ]
  | _ -> []

let get (ctx : Common.Context.t)
    (res : (Rsvp.rsvp option, Error.t option) result) =
  let rsvp =
    match res with
    | Ok (Some rsvp) -> Wire.Strings.Rsvp.encode rsvp |> Rsvp.map snd
    | _ ->
        ({
           user_id = "";
           status = "";
           arriving_from = "";
           returning_to = "";
           transport = "";
           dietary = "";
           plus_one = "";
         }
          : string Rsvp.dict)
  in

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
       Rsvp.fold
         (fun (v, ((label, name), (kind, opts, row, col))) acc ->
           cel ~row ~col
             [
               (match kind with
               | `Select ->
                   Forms.radio ~title:label
                     ~a:[ style @@ with_err name res ]
                     ~checked:v name
                     (List.map
                        (fun (name, value) -> ({ name; value } : Forms.selopt))
                        opts)
               | (`Text | `Number) as kind ->
                   Forms.field ~title:label
                     ~a_input:
                       [
                         (let open Wire.Strings.Rsvp in
                          a_placeholder
                          @@
                          match name with
                          | name
                            when name = fields.arriving_from
                                 || name = fields.returning_to ->
                              "E.g. \"London\", \"Milano\""
                          | _ -> "");
                         style @@ with_err name res;
                         a_value v;
                       ]
                     name kind
               | `Checkbox ->
                   flex ~dir:Row
                     ~a:[ a_class Toggle.container_cl ]
                     ~style:
                       [ ("align-items", "end"); Css.gap @@ Css.var "gap-sm" ]
                     [
                       Forms.checkbox
                         ~a:
                           (Toggle.ctrl ()
                           @ [ Css.style [ ("flex-grow", "1") ] ])
                         ~checked:(v <> "") ~title:label "";
                       Forms.field
                         ~a:
                           [
                             a_class Toggle.on_cl;
                             Css.style [ ("flex-grow", "1") ];
                           ]
                         ~a_input:
                           [
                             a_placeholder "Tell us who you'd like to bring";
                             a_value v;
                           ]
                         name `Text;
                     ]
               | `Hidden -> Forms.field ~a_input:[ a_value "1" ] name `Hidden);
             ]
           :: acc)
         []
         (Rsvp.zip fields inputs |> Rsvp.zip rsvp)
       |> grid ~cols:"auto" ~style:[ gap @@ var "gap-xl" ];
     ]
  @ [ Common.Action.field ~kind:Create () ]

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
