open Tyxml.Html
open Wire.Strings

type t = {
  target : string;
  csrf : string;
  theme : Ctrl.Theme.theme;
  log : 'a. ('a, Format.formatter, unit, unit) format4 -> 'a;
}

let path ?target ctx parts =
  String.concat "/" (Option.value ~default:ctx.target target :: parts)
