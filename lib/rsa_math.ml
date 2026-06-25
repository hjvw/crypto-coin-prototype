let rec ext_gcd a b = 
  if Z.equal b Z.zero then (a, Z.one, Z.zero) 
  else
    let (q, r) = Z.div_rem a b in
    let (g, x, y) = ext_gcd b r in 
    (g, y, Z.sub x (Z.mul q y))

let mod_inverse e phi = 
  let (g, x, _) = ext_gcd e phi in 
  if not (Z.equal g Z.one) then failwith "Modular reversal doesnt exist" 
  else if Z.lt x Z.zero then Z.add x phi else x 

let mod_pow base exp modulus = 
  let rec loop acc b e = 
    if Z.equal e Z.zero then acc 
    else 
      let new_acc = if Z.is_odd e then Z.erem (Z.mul acc b) modulus else acc in 
      let new_b = Z.erem (Z.mul b b) modulus in 
      loop new_acc new_b (Z.div e (Z.of_int 2))
  in 
  loop Z.one base exp 

let miller_rabin_test n a = 
  let n_minus_1 = Z.sub n Z.one in 
  let rec factor_out_twos s d =
    if Z.is_even d then factor_out_twos (s + 1) (Z.div d (Z.of_int 2)) else (s, d)
  in 
  let (s, d) = factor_out_twos 0 n_minus_1 in 
  let x = mod_pow a d n in 
  if Z.equal x Z.one || Z.equal x n_minus_1 then true 
  else
    let rec loop r current_x = 
      if r = s - 1 then false 
      else 
        let next_x = mod_pow current_x (Z.of_int 2) n in 
        if Z.equal next_x n_minus_1 then true 
        else loop (r + 1) next_x 
    in 
  loop 0 x 

let is_prime n k = 
  if Z.leq n  Z.one then false 
  else if Z.leq n (Z.of_int 3) then true 
  else if Z.is_even n then false 
  else 
    let rec check i = 
      if i = 0 then true 
      else 
        let a = Z.of_int (2 + (i mod 10)) in 
        if miller_rabin_test n a then check (i - 1) else false 
    in 
  check k

let () = Random.self_init () 

let generate_random_z bytes_len = 
  let buf = Buffer.create bytes_len in 
  for _ = 1 to bytes_len do 
    Buffer.add_char buf (Char.chr (Random.int 256))
  done; 
  Z.of_bits (Buffer.contents buf)
  
let rec generate_prime bits = 
  let bytes_len = max 1 (bits / 8) in 
  let candidate = Z.logor (generate_random_z bytes_len) Z.one in 
  if is_prime candidate 20 then candidate
  else generate_prime bits 




