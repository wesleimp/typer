(**)
(* let () = *)

open Minttea
open Leaves
open Typer_lib

let init _ = Command.Noop

type model = {
  progress_bar : Progress.t;
  cursor : int;
  text : string list;
  typed : string list;
}

let explode_string s =
  let chars = List.init (String.length s) (String.get s) in
  List.map Char.escaped chars

let initial_model =
  let words = Words.random 5 in
  let text = explode_string (String.concat " " words |> String.trim) in
  {
    cursor = 0;
    text;
    typed = [];
    progress_bar =
      Progress.make ~width:100
        ~color:(`Plain (Spices.color "#3fa2a3"))
        ~trail_char:" " ();
  }

let handle_key_down m key =
  let typed = m.typed @ [ key ] in
  let amount =
    float_of_int (List.length typed) /. float_of_int (List.length m.text)
  in
  let progress_bar = Progress.set_progress m.progress_bar amount in
  {progress_bar; cursor = m.cursor + 1; typed; text = m.text}

let update event m =
  match event with
  | Event.KeyDown Escape -> (m, Command.Quit)
  | Event.KeyDown Space -> (handle_key_down m " ", Command.Noop)
  | Event.KeyDown (Key k) -> (handle_key_down m k, Command.Noop)
  | _ -> (m, Command.Noop)

let view m =
  Format.sprintf "\n\n%s\n\n%s\n\n%s\n"
    (Progress.view m.progress_bar)
    (String.concat "" m.text) (String.concat "" m.typed)

let () = Minttea.app ~init ~update ~view () |> Minttea.start ~initial_model
