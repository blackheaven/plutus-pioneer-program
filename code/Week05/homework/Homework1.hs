{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Homework1 where

import           Plutus.V1.Ledger.Interval (contains)
import           Plutus.V2.Ledger.Api (BuiltinData, MintingPolicy, POSIXTime,
                                       PubKeyHash, ScriptContext,
                                       ScriptContext (scriptContextTxInfo),
                                       TxInfo (..), to,
                                       mkMintingPolicyScript)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import qualified PlutusTx
import           PlutusTx.Prelude     (Bool, ($), (&&))
import           Utilities            (wrapPolicy)

{-# INLINABLE mkDeadlinePolicy #-}
-- This policy should only allow minting (or burning) of tokens if the owner of the specified PubKeyHash
-- has signed the transaction and if the specified deadline has not passed.
mkDeadlinePolicy :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkDeadlinePolicy beneficiary deadline () ctx = signedByBeneficiary && deadlineReached
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info beneficiary

    deadlineReached :: Bool
    deadlineReached = contains (to deadline) $ txInfoValidRange info

{-# INLINABLE mkWrappedDeadlinePolicy #-}
mkWrappedDeadlinePolicy :: PubKeyHash -> POSIXTime -> BuiltinData -> BuiltinData -> ()
mkWrappedDeadlinePolicy pkh deadline = wrapPolicy $ mkDeadlinePolicy pkh deadline

deadlinePolicy :: PubKeyHash -> POSIXTime -> MintingPolicy
deadlinePolicy pkh deadline = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkWrappedDeadlinePolicy ||])
        `PlutusTx.applyCode` PlutusTx.liftCode pkh
        `PlutusTx.applyCode` PlutusTx.liftCode deadline
