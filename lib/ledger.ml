module LedgerMap = Map.Make(String)
type t = float LedgerMap.t 
let empty : t = LedgerMap.empty
let get_balance (account : string) (ledger : t) : float =
  LedgerMap.find_opt account ledger |> Option.value ~default:100.0 
let update_balance (account : string) (amount : float) (ledger : t) : t = 
  LedgerMap.add account amount ledger
let apply_transaction (ledger : t) (tx : Transaction.t) : t = 
  let sender_bal = get_balance tx.sender ledger in 
  match Transaction.is_valid tx sender_bal with 
  | Ok () ->
      let receiver_bal = get_balance tx.receiver ledger in 
      ledger 
      |> update_balance tx.sender (sender_bal -. tx.amount) 
      |> update_balance tx.receiver (receiver_bal +. tx.amount) 
  | Error _ -> ledger 
let apply_block_transactions (ledger : t) (txs : Transaction.t list) : t =
  List.fold_left apply_transaction ledger txs
