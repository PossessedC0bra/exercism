let to_chars (s: string) : char list = List.init (String.length s) (String.get s)
let to_string (cs: char list) : string = List.fold_left (fun acc c -> (Buffer.add_char acc c); acc) (Buffer.create (List.length cs)) cs |> Buffer.contents

let is_word c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
let is_word_seperator c = c = ' ' || c = '-'

let sanitize (c: char) : char option = match (is_word c, is_word_seperator c) with
  | (true, _) -> Some (c)
  | (_, true) -> Some (' ')
  | _ -> None

let acronym s = to_chars s 
|> List.filter_map sanitize
|> to_string
|> String.split_on_char ' '
|> List.filter (fun word -> String.length word > 0)
|> List.map (fun word -> word.[0] |> Char.uppercase_ascii)
|> to_string