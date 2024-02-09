let random size =
  let list_len = List.length Wordlist.words in
  let rec random_word remaining acc =
    match remaining with
    | 0 -> acc
    | n ->
      Random.self_init ();
      let word = List.nth Wordlist.words (Random.int list_len) in
      random_word (n - 1) (acc @ [ word ])
  in
  random_word size []
