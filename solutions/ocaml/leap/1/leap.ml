let leap_year x = 
    match x with
    | _ when x mod 4 == 0 -> if x mod 100 == 0 then x mod 400 == 0 else true
    | _ -> false
