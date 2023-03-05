#!/bin/bash

assets=/work/code/Week02/experiments
keypath=/work/keys
name=alice
txin=21acf492f4a19bd695fac8a3576eebce98e5f2b6c45080088656e44f6432efc6#0
body="$assets/factoring.txbody"
tx="$assets/factoring.tx"

# Build factoring script address 
cardano-cli address build \
    --payment-script-file "$assets/factoring.plutus" \
    --testnet-magic 2 \
    --out-file "$assets/factoring.addr"

# Build the transaction
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 2 \
    --tx-in "$txin" \
    --tx-out "$(cat "$assets/factoring.addr") + 3000000 lovelace" \
    --tx-out-inline-datum-file "$assets/datum.json" \
    --change-address "$(cat "$keypath/$name.addr")" \
    --out-file "$body"
    
# Sign the transaction
cardano-cli transaction sign \
    --tx-body-file "$body" \
    --signing-key-file "$keypath/$name.skey" \
    --testnet-magic 2 \
    --out-file "$tx"

# Submit the transaction
cardano-cli transaction submit \
    --testnet-magic 2 \
    --tx-file "$tx"

tid=$(cardano-cli transaction txid --tx-file "$tx")
echo "transaction id: $tid"
echo "Cardanoscan: https://preview.cardanoscan.io/transaction/$tid"
