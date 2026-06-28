let rec parse_tx (json_obj : Yojson.Basic.t) : Model.transaction =
  let open Yojson.Basic.Util in
  {
    Model.sender = json_obj |> member "sender" |> to_string;
    Model.receiver = json_obj |> member "receiver" |> to_string;
    Model.amount = json_obj |> member "amount" |> to_float;
    Model.nonce = json_obj |> member "nonce" |> to_int;
    Model.signature = json_obj |> member "signature" |> to_string;
  }

let tx_to_json (t : Model.transaction) =
  `Assoc [
    ("sender", `String t.Model.sender);
    ("receiver", `String t.Model.receiver);
    ("amount", `Float t.Model.amount);
    ("nonce", `Int t.Model.nonce);
    ("signature", `String t.Model.signature);
  ]

let parse_block (json : Yojson.Basic.t) : Model.block =
  let open Yojson.Basic.Util in
  {
    Model.index = json |> member "index" |> to_int;
    Model.timestamp = json |> member "timestamp" |> to_float;
    Model.prev_hash = json |> member "prev_hash" |> to_string;
    Model.hash = json |> member "hash" |> to_string;
    Model.nonce = json |> member "nonce" |> to_int;
    Model.transactions = json |> member "transactions" |> to_list |> List.map parse_tx;
  }

let block_to_json (b : Model.block) =
  `Assoc [
    ("index", `Int b.Model.index);
    ("timestamp", `Float b.Model.timestamp);
    ("prev_hash", `String b.Model.prev_hash);
    ("hash", `String b.Model.hash);
    ("nonce", `Int b.Model.nonce);
    ("transactions", `List (List.map tx_to_json b.Model.transactions));
  ]
