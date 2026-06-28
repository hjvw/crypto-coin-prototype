open Lwt.Infix
open Cohttp_lwt_unix

let json_response status json_obj = 
  let body = Yojson.Basic.to_string json_obj in 
  Server.respond_string ~status ~body () 

let broadcast_block (new_block : Model.block) = 
  let block_json = Json.block_to_json new_block in 
  let body_str = Yojson.Basic.to_string block_json in 

  Lwt_list.iter_p (fun peer_url ->
    let uri = Uri.of_string (peer_url ^ "/p2p/block") in 
    let headers = Cohttp.Header.init_with "Content-Type" "application/json" in 
    Lwt.catch 
      (fun () ->
        Client.post ~headers ~body:(Cohttp_lwt.Body.of_string body_str) uri >>= fun (resp, _body) -> 
        let code = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in 
        Lwt_io.printlf "[P2P] Broadcasted to %s, status: %d" peer_url code 
      )
      (fun exn -> 
        Lwt_io.printlf "[P2P] Peer error %s: %s" peer_url (Printexc.to_string exn)
      )
    ) !Types.peers 

let callback _conn req body =
  let uri = Cohttp.Request.uri req in
  let path = Uri.path uri in
  let meth = Cohttp.Request.meth req in

  match (meth, path) with
  | (`POST, "/transaction") ->
      Cohttp_lwt.Body.to_string body >>= fun body_str ->
      Lwt.catch
      (fun () ->
        let json = Yojson.Basic.from_string body_str in
        let tx = Json.parse_tx json in
        let sender_balance = Ledger.get_balance tx.Model.sender !Types.current_ledger in
        match Transaction.is_valid tx sender_balance with
        | Ok () ->
          Types.mempool := tx :: !Types.mempool;
          json_response `Created (`Assoc [("status", `String "Added to mempool")])
        | Error msg ->
          json_response (`Code 400) (`Assoc [("error", `String msg)]))
      (fun _exn ->
        json_response (`Code 400)(`Assoc [("error", `String "Invalid JSON format")])
      )

  | (`GET, "/blocks") ->
      let block_json = `List (List.map Json.block_to_json !Types.blockchain) in
      json_response (`Code 200) block_json

  | (`GET, path) when String.starts_with ~prefix:"/balance/" path ->
      let addr = String.sub path 9 (String.length path - 9) in
      let balance = Ledger.get_balance addr !Types.current_ledger in
      json_response (`Code 200) (`Assoc [("address", `String addr); ("balance", `Float balance)])

  | (`POST, "/p2p/block") ->
      Cohttp_lwt.Body.to_string body >>= fun body_str ->
      Lwt.catch
      (fun () ->
        let json = Yojson.Basic.from_string body_str in
        let incoming_block = Json.parse_block json in
        let difficulty = "00" in
        let is_block_ok = Consensus.validate incoming_block !Types.blockchain difficulty in
        if is_block_ok then begin
          Types.blockchain := incoming_block :: !Types.blockchain;
          Types.current_ledger := Ledger.apply_block_transactions !Types.current_ledger incoming_block.Model.transactions;
          json_response `Created (`Assoc [("status", `String "Block integrated")])
        end else
          json_response (`Code 400) (`Assoc [("error", `String "Validation failed")]))
      (fun _exn -> json_response (`Code 400) (`Assoc [("error", `String "Malformed block")]))

  | (`POST, "/user") ->
      Cohttp_lwt.Body.to_string body >>= fun body_str ->
      Lwt.catch
      (fun () ->
        let json = Yojson.Basic.from_string body_str in
        let open Yojson.Basic.Util in
        let bits = json |> member "bits" |> to_int in
        let keys = Rsa.generate_keys bits in
        let public_address = Z.to_string keys.pub.n in
        Types.current_ledger := Ledger.update_balance public_address 100.0 !Types.current_ledger;
        json_response (`Code 201) (`Assoc [("status", `String "Keys generated succesfully"); ("account_address", `String public_address)])
      )
      (fun _exc -> json_response (`Code 400) (`Assoc [("error", `String "Invalid JSON format or bits field")]))

  | (`POST, "/mine") ->
      let txs = !Types.mempool in
      if txs = [] then
        json_response (`Code 400) (`Assoc [("error", `String "Mempool is empty")])
      else
        let last_block = List.hd !Types.blockchain in
        let difficulty = "00" in
        let block_data = string_of_int (last_block.Model.index + 1) ^ last_block.Model.hash in
        let nonce = Consensus.mine_block block_data difficulty in
        let final_hash = Utils.to_hex (Sha256.string (block_data ^ string_of_int nonce)) in
        let mined_block = {
          Model.index = last_block.Model.index + 1;
          Model.timestamp = Unix.time ();
          Model.transactions = txs;
          Model.prev_hash = last_block.Model.hash;
          Model.nonce = nonce;
          Model.hash = final_hash
        } in
        Types.blockchain := mined_block :: !Types.blockchain;
        Types.current_ledger := Ledger.apply_block_transactions !Types.current_ledger txs;
        Types.mempool := [];
        json_response `Created (`Assoc [("status", `String "Block mined!"); ("hash", `String final_hash); ("nonce", `Int nonce)])

  | _ -> json_response (`Code 404) (`Assoc [("error", `String "Not Found")])

let start_server port = 
  let mode = `TCP (`Port port) in 
  let config = Server.make ~callback () in 
  Server.create ~mode config
