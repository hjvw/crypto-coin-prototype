let to_hex s = 
  String.fold_left (fun acc c -> acc ^ Printf.sprintf "%02x" (Char.code c)) "" s
