open Base

let empty = Map.empty (module Char)

let nucleotides = "ACGT"
let is_nucleotide = String.contains nucleotides

(* override = operator to avoid type errors *)
let (=) = Char.equal

(* val count_nucleotides : string -> (int Map.M(Char).t, char) Result.t *)
let count_nucleotides = 
  String.fold_result 
  ~init:empty 
  ~f:(fun acc c -> 
    if is_nucleotide c 
    then Ok (Map.update acc c ~f:(fun v -> match v with | Some (i) -> i + 1 | None -> 1))
    else Error c
  )

(* val count_nucleotide : string -> char -> (int, char) Result.t *)
let count_nucleotide s c =
  match is_nucleotide c with
    | false -> Error c
    | _ -> match count_nucleotides s with
      | Error c' -> Error c'
      | Ok map -> match Map.find map c with
        | Some i -> Ok i
        | None -> Ok 0