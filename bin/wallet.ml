
let d = Z.of_string Sys.argv.(1)
let n = Z.of_string Sys.argv.(2)
let receiver = Sys.argv.(3)
let amount_str = Sys.argv.(4)

let msg_z = Z.of_string (string_of_int (Hashtbl.hash (receiver ^ amount_str)))

let sig_z = Z.powm msg_z d n

let () = print_string (Z.format "%x" sig_z)
