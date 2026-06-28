type tx = Model.transaction 
type block = Model.block 

let start_node = Network.start_server 
let broadcast_new_block = Network.broadcast_block 
let set_peers list_of_peers = Types.peers := list_of_peers 
let get_mempool () = !Types.mempool 
let get_blockchain () = !Types.blockchain
