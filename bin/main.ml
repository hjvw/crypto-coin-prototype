let test_hash msg expected = 
  let result = Kukrycoin.Sha256.string msg in 
  Printf.printf "tekst: \"%s\"\n" msg;
  Printf.printf "wynik: %s\n" result;
  Printf.printf "Expected: %s\n" expected
let () = 
  print_endline "test";

  test_hash "abc" "dwadad";

  print_endline "generating key pair";
  let keys = Kukrycoin.Rsa.generate_keys 2048 in 
  print_endline "keys generated";

  let message = Bytes.of_string "abc" in 
  Printf.printf "Original message : \"%s\"\n" (Bytes.to_string message);

  print_endline "ecrypting with public key";
  let cipher = Kukrycoin.Rsa.encrypt keys.pub message in
  let bytes_to_hex b = 
    let buf = Buffer.create (Bytes.length b * 2) in 
    Bytes.iter (fun c -> Printf.bprintf buf "%02x" (Char.code c)) b; 
    Buffer.contents buf
  in
  Printf.printf "Hexated message: %s\n" (bytes_to_hex cipher);

  print_endline "Deciphring with private key";
  let decipher = Kukrycoin.Rsa.decrypt keys.priv cipher in 
  Printf.printf "Deciphring result: \"%s\"\n" (Bytes.to_string decipher);

