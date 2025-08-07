let raindrop n = 
  let str = (if n mod 3 = 0 then "Pling" else "") ^ (if n mod 5 = 0 then "Plang" else "") ^ (if n mod 7 = 0 then "Plong" else "") in
  if String.length str = 0
    then Int.to_string n
    else str