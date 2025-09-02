module CharMap = Map.Make(Char)

let count_character_frequency = 
  String.fold_left (fun acc c -> CharMap.update c (fun v -> match v with | Some(n) -> Some(n + 1) | None -> Some(1)) acc) CharMap.empty

let anagrams word words =
  let reference_character_frequency = count_character_frequency (String.lowercase_ascii word) in
  words 
  |> List.map (fun word -> (word, count_character_frequency (String.lowercase_ascii word)))
  |> List.filter (fun (_, char_freq) ->  CharMap.equal Int.equal char_freq reference_character_frequency) 
  |> List.map (fun (word, _) -> word)
  |> List.filter (fun w -> not (String.equal (String.lowercase_ascii word) (String.lowercase_ascii w)))