let explode (s: string) : char list = List.init (String.length s) (fun i -> s.[i]);;
let implode (cs: char list) : string = 
    let buffer = List.fold_left
        (fun acc c -> Buffer.add_char acc c ; acc)
        (Buffer.create (List.length cs))
        cs
    in
    Buffer.contents buffer

let atbash (c:char) : char = 
    Char.code c
    |> (Fun.flip (-)) (Char.code 'z')
    |> abs
    |> (Fun.flip (mod)) 26
    |> (+) (Char.code 'a')
    |> Char.chr
;;

let interperse (n: int) (a: 'a) (l: 'a list): 'a list = 
    let (list, _) = List.fold_left
        (fun (acc, c) e -> if c = n then (e:: a :: acc, 1) else (e :: acc, c + 1))
        ([], 0)
        l
    in
    List.rev list
;;

let encode ?(block_size = 5) s = 
    String.lowercase_ascii s
    |> explode
    |> List.filter_map 
        (fun c -> match c with
            | c when 'a' <= c && c <= 'z' -> Some (atbash c)
            | c when '0' <= c && c <= '9' -> Some (c)
            | _ -> None
        )
    |> interperse block_size ' '
    |> implode
;;

let decode s = 
    String.split_on_char ' ' s
    |> List.map (explode)
    |> List.flatten
    |> List.filter_map 
        (fun c -> 
            match c with
            | c when 'a' <= c && c <= 'z' -> Some (atbash c)
            | c when '0' <= c && c <= '9' -> Some (c)
            | _ -> None
        )
    |> implode
;;