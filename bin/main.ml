(**)
(* let () = *)

open Minttea
open Leaves
open Typer_lib

let init _ = Command.Noop

type model = {
  progress_bar : Progress.t;
  words : string list;
  cursor : int;
}

let initial_model =
  let words = Words.random 15 in
  let width = String.length (String.concat " " words) in
  {
    cursor = 0;
    words = Words.random 15;
    progress_bar =
      Progress.make ~width ~color:(`Plain (Spices.color "#3fa2a3")) ();
  }

let update event m =
  match event with
  | Event.KeyDown (Key "q" | Escape) -> (m, Command.Quit)
  (* | Event.Frame _now -> *)
  (*   let progress_bar = Progress.increment m.progress_bar 0.00001 in *)
  (*   ({progress_bar;}, Command.Noop) *)
  | _ -> (m, Command.Noop)

let view m =
  Format.sprintf "\n\n%s\n\n%s\n\n"
    (Progress.view m.progress_bar)
    (String.concat " " m.words)

let () = Minttea.app ~init ~update ~view () |> Minttea.start ~initial_model
