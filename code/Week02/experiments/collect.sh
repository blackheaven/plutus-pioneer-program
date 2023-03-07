#!/bin/bash

assets=/work/code/Week02/experiments
keypath=/work/keys
name=bob
# Bob's 1st TxHash
collateral="2e1fe0fe79677804a70bff96b249d9912143a3464f4788d7a42b1f772bfb2050#0"
# Contract's 1st TxHash
txin="85d0de72d843495bcbb20e061efbe67d6c121a5c26fd3b5da5b63e9d0310930f#0"
version="$1"

pp="$assets/$version-protocol-parameters.json"
body="$assets/$version-collect-factoring.txbody"
tx="$assets/$version-collect-factoring.tx"

echo "# Query the protocol parameters"

cardano-cli query protocol-parameters \
    --testnet-magic 2 \
    --out-file "$pp"

echo "============"

echo "# Build the transaction"
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 2 \
    --tx-in "$txin" \
    --tx-in-script-file "$assets/factoring.plutus" \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file "$assets/$version-redeemer.json" \
    --tx-in-collateral "$collateral" \
    --change-address "$(cat "$keypath/$name.addr")" \
    --protocol-params-file "$pp" \
    --out-file "$body"

echo "============"
    
echo "# Sign the transaction"
cardano-cli transaction sign \
    --tx-body-file "$body" \
    --signing-key-file "$keypath/$name.skey" \
    --testnet-magic 2 \
    --out-file "$tx"

echo "============"

echo "# Submit the transaction"
cardano-cli transaction submit \
    --testnet-magic 2 \
    --tx-file "$tx"

tid=$(cardano-cli transaction txid --tx-file "$tx")
echo "transaction id: $tid"
echo "Cardanoscan: https://preview.cardanoscan.io/transaction/$tid"
