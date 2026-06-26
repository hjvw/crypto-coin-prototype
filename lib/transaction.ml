type t = { 
  sender : string;
  receiver : string; 
  amount : float; 
  nonce : int;
  signature : string;
}

let is_valid (tx : t) (sender_balance : float) : bool = 
  let funds_ok = sender_balance >= tx.amount in 
  let amount_ok = tx.amount > 0.0 in 
  funds_ok && amount_ok
