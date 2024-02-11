open Minttea
open Leaves
open Typer_lib

let init _ = Command.Noop

type model = {
  progress_bar : Progress.t;
  text : string list;
  typed : string list;
}

let explode_string s =
  let chars = List.init (String.length s) (String.get s) in
  List.map Char.escaped chars

let initial_model =
  let words = Words.random 1 in
  let text = explode_string (String.concat " " words |> String.trim) in
  {
    text;
    typed = [];
    progress_bar =
      Progress.make ~width:100
        ~color:(`Plain (Spices.color "#4776E6"))
        ~trail_char:" " ();
  }

let handle_key_down m key =
  let typed = m.typed @ [ key ] in
  let percentage_of x y = Float.div (float_of_int x) (float_of_int y) in
  let progress = percentage_of (List.length typed) (List.length m.text) in
  let progress_bar = Progress.set_progress m.progress_bar progress in
  {progress_bar; typed; text = m.text}

let update event m =
  match event with
  | Event.KeyDown Escape -> (m, Command.Quit)
  | Event.KeyDown Space -> (handle_key_down m " ", Command.Noop)
  | Event.KeyDown (Key k) -> (handle_key_down m k, Command.Noop)
  | _ -> (m, Command.Noop)

let view m =
  let typed_len = List.length m.typed in
  let remaining = m.text |> List.to_seq |> Seq.drop typed_len |> List.of_seq in
  let typed =
    m.typed
    |> List.mapi (fun i ch ->
           if String.equal ch (List.nth m.text i) then
             Format.sprintf "%s" (Styles.typed ch)
           else
             Format.sprintf "%s" (Styles.error ch))
    |> String.concat ""
  in
  let untyped =
    remaining
    |> List.map (fun ch -> Format.sprintf "%s" (Styles.untyped ch))
    |> String.concat ""
  in
  Format.sprintf "\n\n%s\n\n%s%s\n\n"
    (Progress.view m.progress_bar)
    typed untyped

let () = Minttea.app ~init ~update ~view () |> Minttea.start ~initial_model
