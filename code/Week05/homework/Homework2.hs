{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Homework2 where

import           Plutus.V1.Ledger.Value (flattenValue)
import           Plutus.V2.Ledger.Api   (BuiltinData, MintingPolicy,
                                         ScriptContext (scriptContextTxInfo),
                                         TokenName (..), TxOutRef,
                                         TxInInfo (txInInfoOutRef),
                                         TxInfo (..),
                                         mkMintingPolicyScript)
import qualified PlutusTx
import           PlutusTx.Prelude       (Bool (..), Eq ((==)), any,
                                         traceIfFalse, ($), (&&), (.))
import           Utilities              (wrapPolicy)

{-# INLINABLE mkEmptyNFTPolicy #-}
-- Minting policy for an NFT, where the minting transaction must consume the given UTxO as input
-- and where the TokenName will be the empty ByteString.
mkEmptyNFTPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkEmptyNFTPolicy oref () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                               traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn'', amt)] -> tn'' == policyTokenName && amt == 1
        _                -> False

{-# INLINABLE policyTokenName #-}
policyTokenName :: TokenName
policyTokenName = TokenName ""

{-# INLINABLE mkWrappedEmptyNFTPolicy #-}
mkWrappedEmptyNFTPolicy :: TxOutRef -> BuiltinData -> BuiltinData -> ()
mkWrappedEmptyNFTPolicy = wrapPolicy . mkEmptyNFTPolicy

nftPolicy :: TxOutRef -> TokenName -> MintingPolicy
nftPolicy oref tn = mkMintingPolicyScript $ $$(PlutusTx.compile [|| mkWrappedEmptyNFTPolicy ||]) `PlutusTx.applyCode` PlutusTx.liftCode oref
