module LedgerMap = Map.Make(String)

type t = float LedgerMap.t 

let empty : t = LedgerMap.empty

let get_balance (account : string) (ledger : t) : float =
  LedgerMap.find_opt account ledger |> Option.value ~default:0.0 

let apply_transaction (ledger : t) (tx : Transaction.t) : t = 
  let sender_bal = get_balance tx.sender ledger in 

  if Transaction.is_valid tx sender_bal then 
    let receiver_bal = get_balance tx.receiver ledger in 
    ledger 
    |> LedgerMap.add tx.sender (sender_bal -. tx.amount) 
    |> LedgerMap.add tx.receiver (receiver_bal +. tx.amount) 
  else ledger 

let apply_block_transactions (ledger : t) (txs : Transaction.t list) : t =
  List.fold_left apply_transaction ledger txs 
