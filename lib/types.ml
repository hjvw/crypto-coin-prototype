let mempool = ref ([] : Model.transaction list)
let blockchain = ref ([] : Model.block list)
let current_ledger = ref (Ledger.empty
  |> Ledger.LedgerMap.add "Jan" 100.0
  |> Ledger.LedgerMap.add "Bob" 50.0)
let peers = ref ([] : string list)
