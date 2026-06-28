type t = Model.transaction

let is_valid (tx : t) (sender_balance : float) : (unit, string) result =
  if sender_balance < tx.amount then 
    Error "Insufficient funds"
  else 
    try 
      let sig_z = Z.of_string_base 16 tx.signature in
      let n = Z.of_string tx.sender in
      let e = Z.of_int 65537 in
      let decrypted_z = Z.powm sig_z e n in
      let msg = tx.receiver ^ (Printf.sprintf "%.1f" tx.amount) ^ (string_of_int tx.nonce) in
      let hash_hex = Utils.to_hex (Sha256.string msg) in
      let expected_z = Z.of_string_base 16 hash_hex in
      if Z.equal decrypted_z expected_z then Ok ()
      else Error "Signature mismatch!"
    with | _ -> Error "Invalid signature format"
