 open Int32


let process_block (block : bytes) (offset : int) (h_state : int32 array) =
  let w = Array.make 64 0l in 

  for i = 0 to 15 do 
    let idx = offset + (i * 4) in 
    let b0 = Int32.of_int (Char.code (Bytes.get block idx)) in 
    let b1 = Int32.of_int (Char.code (Bytes.get block (idx + 1))) in 
    let b2 = Int32.of_int (Char.code (Bytes.get block (idx + 2))) in 
    let b3 = Int32.of_int (Char.code (Bytes.get block (idx + 3))) in 
    w.(i) <- logor (shift_left b0 24) (logor (shift_left b1 16) (logor (shift_left b2 8) b3))
  done; 


  for i = 16 to 63 do 
    w.(i) <- add (add (Sha256_ops.sigma1 w.(i - 2)) w.(i - 7)) (add (Sha256_ops.sigma0 w.(i - 15)) w.(i - 16))
  done; 

  let a = ref h_state.(0) in 
  let b = ref h_state.(1) in 
  let c = ref h_state.(2) in 
  let d = ref h_state.(3) in 
  let e = ref h_state.(4) in 
  let f = ref h_state.(5) in 
  let g = ref h_state.(6) in 
  let h_var = ref h_state.(7) in 


  for i = 0 to 63 do 
    let t1 = add (add (add !h_var (Sha256_ops.big_sigma1 !e)) (Sha256_ops.ch !e !f !g)) (add Sha256_ops.k.(i) w.(i)) in 
    let t2 = add (Sha256_ops.big_sigma0 !a) (Sha256_ops.maj !a !b !c) in 

    h_var := !g;
    g := !f;
    f := !e;
    e := add !d t1;
    d := !c; 
    c := !b; 
    b := !a;
    a := add t1 t2 
  done;



  h_state.(0) <- add h_state.(0) !a;
  h_state.(1) <- add h_state.(1) !b;
  h_state.(2) <- add h_state.(2) !c;
  h_state.(3) <- add h_state.(3) !d;
  h_state.(4) <- add h_state.(4) !e;
  h_state.(5) <- add h_state.(5) !f;
  h_state.(6) <- add h_state.(6) !g;
  h_state.(7) <- add h_state.(7) !h_var




let string (msg : string) : string = 
  let h_state = [|
    0x6a09e667l; 0xbb67ae85l; 0x3c6ef372l; 0xa54ff53al;
    0x510e527fl; 0x9b05688cl; 0x1f83d9abl; 0x5be0cd19l;
  |] in 


  let padded = Sha256_pad.pad_message msg in 
  let num_blocks = Bytes.length padded / 64 in 
  

  for b = 0 to num_blocks - 1 do 
    process_block padded (b * 64) h_state
  done; 

  let buf = Buffer.create 64 in

  Array.iter (fun x ->
    let clean_int = (Int32.to_int x) land 0xFFFFFFFF in 
    Printf.bprintf buf "%08x" clean_int)  h_state;
  Buffer.contents buf


