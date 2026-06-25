type public_key = { e : Z.t; n : Z.t }
type private_key = { d : Z.t; n : Z.t }
type key_pair = { pub : public_key; priv : private_key }

val generate_keys : int -> key_pair 

val encrypt : public_key -> bytes -> bytes 

val decrypt : private_key -> bytes -> bytes 
