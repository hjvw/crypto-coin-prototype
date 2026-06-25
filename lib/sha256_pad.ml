open Int64
let pad_message (msg : string) : bytes =
  let orig_len = String.length msg in 
  let orig_bits = Int64.mul (Int64.of_int orig_len) 8L in 

  let k = (448 - ((orig_len * 8) + 8) mod 512) mod 512 in 

  let pad_len = orig_len + 1 + (k / 8) + 8 in 

  let buf = Bytes.create pad_len in 

  Bytes.blit_string msg 0 buf 0 orig_len;

  Bytes.set buf orig_len (Char.chr 0x80);

  Bytes.fill buf (orig_len + 1) (k / 8) (Char.chr 0x00);

  let start_offset = pad_len - 8 in 
  for i = 0 to 7 do 
    let shift = (7 - i) * 8 in 
    let byte = Int64.to_int (Int64.logand (Int64.shift_right_logical orig_bits shift) 0xFFL) in 
    Bytes.set buf (start_offset + i) (Char.chr byte)
  done; 

  buf
