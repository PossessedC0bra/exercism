let to_string (cs: char list) : string = List.fold_left (fun acc c -> (Buffer.add_char acc c); acc) (Buffer.create (List.length cs)) cs |> Buffer.contents

let is_word_character c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c = '\''

let acronym s = String.uppercase_ascii s
|> String.map (fun c -> if is_word_character c || c = '\'' then c else ' ')
|> String.split_on_char ' '
|> List.filter (fun word -> String.length word > 0)
|> List.map (fun word -> word.[0])
|> to_string