

type public_key = { e : Z.t; n : Z.t }
type private_key = { d : Z.t; n : Z.t }
type key_pair = { pub : public_key; priv : private_key }

let generate_keys bits = 
  let p = Rsa_math.generate_prime (bits / 2) in 
  let q = Rsa_math.generate_prime (bits / 2) in 
  let n = Z.mul p q in 
  let phi = Z.mul (Z.sub p Z.one) (Z.sub q Z.one) in 
  let e = Z.of_int 65537 in 
  let d = Rsa_math.mod_inverse e phi in 
  { pub = { e = e; n = n }; priv = { d = d; n = n } }


let bytes_to_z b = 
  Z.of_bits (Bytes.to_string b)

let z_to_bytes z = 
  Bytes.of_string (Z.to_bits z) 

let encrypt (pub_key :public_key) message_bytes = 
  let m = bytes_to_z message_bytes in 
  if Z.geq m pub_key.n then failwith "Message too long for this key"; 
  let c = Rsa_math.mod_pow m pub_key.e pub_key.n in 
  z_to_bytes c 

let decrypt (priv_key : private_key) ciphertext_bytes = 
  let c = bytes_to_z ciphertext_bytes in 
  let m = Rsa_math.mod_pow c priv_key.d priv_key.n in 
  z_to_bytes m
