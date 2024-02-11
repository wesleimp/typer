let typed ch = Spices.(default |> fg (Spices.color "#ffffff") |> build) "%s" ch

let error ch =
  Spices.(
    default
    |> fg (Spices.color "#ffffff")
    |> bg (Spices.color "#ff3333")
    |> build)
    "%s" ch

let untyped ch =
  Spices.(default |> fg (Spices.color "#555555") |> build) "%s" ch
