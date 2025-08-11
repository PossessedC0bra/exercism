let encode s = 
  let buffer = Buffer.create (String.length s) in
  let flush c count =
    if count > 1 then Buffer.add_string buffer (Int.to_string count) else () ; 
    Buffer.add_char buffer c
  in
  let (ch, count) = String.fold_left
    (fun (ch, count) current_char -> match ch with
      | None -> (Some current_char, 1)
      | Some (c) when c = current_char -> (Some c, count + 1)
      | Some (c) -> flush c count ; 
        (Some current_char, 1)
    )
    (None, 0)
    s
  in
  (match (ch) with
    | Some c -> flush c count
    | None -> ()
  );
  Buffer.contents buffer
  ;;

let decode s = 
  let buffer = Buffer.create (String.length s) in
  let flush c count = match count with
    | Some (count) -> Buffer.add_string buffer (String.init count (fun _ -> c))
    | None -> Buffer.add_char buffer c
  in
  let is_number c = '0' <= c && c <= '9' in
  let is_letter c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c = ' ' in
  let to_integer c = Char.code c - Char.code '0' in
  let _ = String.fold_left
    (fun count current_char -> 
      if is_number current_char then
        begin
          match count with 
          | Some c -> Some ((c * 10) + (to_integer current_char))
          | None -> Some (to_integer current_char)
        end
      else if is_letter current_char then 
        begin
          flush current_char count ;
          None
        end
      else None
    )
    None
    s
  in
  Buffer.contents buffer
;;
