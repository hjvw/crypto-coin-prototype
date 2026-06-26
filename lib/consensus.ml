let mine_block (data : string) (difficulty : string) : int =

  let to_hex s = 
    String.fold_left (fun acc c -> acc ^ Printf.sprintf "%02x" (Char.code c)) "" s 
  in 


  let rec loop nonce = 
    let raw_hash = Sha256.string (data ^ string_of_int nonce) in
    let hex_hash = to_hex raw_hash in 
    if String.starts_with ~prefix:difficulty hex_hash then nonce 
    else loop (nonce + 1)
  in 
  loop 0
