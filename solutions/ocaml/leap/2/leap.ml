let leap_year x = 
    match x with
    | _ when x mod 400 == 0 -> true
    | _ when x mod 100 == 0 -> false
    | _ -> x mod 4 == 0
    