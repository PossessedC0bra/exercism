type nucleotide = A | C | G | T

let hamming_distance a b =
  let compute_hamming_distance = List.fold_left2 (fun acc a b -> acc + Bool.to_int (a != b)) 0 in
  match a, b with
  | a, b when List.compare_lengths a b == 0 -> Ok (compute_hamming_distance a b)
  | _ -> Error "strands must be of equal length"
