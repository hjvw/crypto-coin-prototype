type transaction = {
  sender : string;
  receiver : string;
  amount : float;
  nonce : int;
  signature : string;
}

type block = {
  index : int;
  timestamp : float;
  transactions : transaction list;
  prev_hash : string;
  hash : string;
  nonce : int;
}
