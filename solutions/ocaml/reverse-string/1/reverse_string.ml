let rec reverse_string s = 
  let length = String.length s in
  String.init length (fun idx -> String.get s (length - idx - 1))
