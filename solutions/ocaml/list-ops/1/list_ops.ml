let length = 
  let rec length' : 'a list -> int = function
    | [] -> 0
    | _::xs -> 1 + length' xs
    in length'
;;

let reverse xs = 
  let rec reverse' (acc: 'a list) : ('a list -> 'a list) = function
    | [] -> acc
    | x::xs -> reverse' (x::acc) xs
    in reverse' [] xs
;;

let map ~f = 
  let rec map' : 'a list -> 'b list = function
    | [] -> []
    | x::xs -> f x :: map' xs
    in map'
;;

let filter ~f = 
  let rec filter' : 'a list -> 'a list = function
    | [] -> []
    | x::xs -> if f x then x :: filter' xs else filter' xs
    in filter'
;;

let fold ~init ~f = 
  let rec fold' acc : 'a list -> 'acc = function
    | [] -> acc
    | x::xs -> let currentVal = f acc x in fold' currentVal xs
    in fold' init
;;

let append l0 l1 = 
  let rec append' : 'a list -> 'a list = function
    | [] -> l1
    | x::xs -> x :: append' xs
  in append' l0
;;

let concat = 
  let rec concat' : 'a list list -> 'a list = function
    | [] -> []
    | x::xs -> append x (concat' xs)
  in concat'
;;