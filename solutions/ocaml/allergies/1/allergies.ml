type allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats 

let allergen_value = function
  | Eggs -> 1
  | Peanuts -> 2
  | Shellfish -> 4
  | Strawberries -> 8
  | Tomatoes -> 16
  | Chocolate -> 32
  | Pollen -> 64
  | Cats -> 128

let allergens = [
  (Eggs, 1);
  (Peanuts, 2);
  (Shellfish, 4);
  (Strawberries, 8);
  (Tomatoes, 16);
  (Chocolate, 32);
  (Pollen, 64);
  (Cats, 128);
]

(* val allergic_to : int -> allergen -> bool *)
let allergic_to score allergen =
  let allergen_score = allergen_value allergen in
  score land (allergen_score) = allergen_score

(* val allergies : int -> allergen list *)
let allergies score = List.filter_map (fun (a, i) -> if allergic_to score a then Some (a) else None) allergens
