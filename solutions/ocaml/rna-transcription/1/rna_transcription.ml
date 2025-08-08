type dna = [ `A | `C | `G | `T ]
type rna = [ `A | `C | `G | `U ]

(* val to_rna : dna list -> rna list *)
let to_rna = 
  let to_rna' = function
    | `A -> `U
    | `C -> `G
    | `G -> `C
    | `T -> `A
  in
  List.map to_rna'