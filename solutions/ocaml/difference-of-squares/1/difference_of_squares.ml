let range t = List.init t (fun x -> x + 1)

let rec sum xs = 
  match xs with
  | [] -> 0
  | x::xs -> x + (sum xs)

let square (x:int) : int= x*x

let square_of_sum n = range n |> sum |> square

let sum_of_squares n = range n |> List.map square |> sum

let difference_of_squares n = square_of_sum n - sum_of_squares n
