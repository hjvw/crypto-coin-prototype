let mine_block (data : string) (difficulty : string) : int =
  let rec loop nonce =
    let raw_hash = Sha256.string (data ^ string_of_int nonce) in
    let hex_hash = Utils.to_hex raw_hash in 
    if String.starts_with ~prefix:difficulty hex_hash then nonce 
    else loop (nonce + 1)
  in loop 0

let check_difficulty hash difficulty = 
  String.starts_with ~prefix:difficulty hash 

let validate (incoming_block : Model.block) (current_chain : Model.block list) difficulty = 
  match current_chain with
  | [] -> true 
  | last_block :: _ ->
      let is_linked = incoming_block.prev_hash = last_block.hash in 
      let is_valid_pow = check_difficulty incoming_block.hash difficulty in 
      is_linked && is_valid_pow
