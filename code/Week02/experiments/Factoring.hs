{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}

module Factoring where

import qualified Plutus.V2.Ledger.Api as PlutusV2
import           PlutusTx             (compile)
import           PlutusTx.Prelude     (Bool, Eq ((==)), Integer, traceIfFalse, traceIfTrue, ($), (||), MultiplicativeSemigroup (..), (&&))
import           Prelude              (IO)
import           Utilities            (wrap, writeValidatorToFile)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

--              Datum  Redeemer        ScriptContext
mkValidator :: Integer -> (Integer, Integer) -> PlutusV2.ScriptContext -> Bool
mkValidator t (x, y) _ = shouldNotBeOne && shouldBeFactors
  where shouldNotBeOne = traceIfTrue "No factor should be 1" $ x == 1 || y == 1
        shouldBeFactors  = traceIfFalse "Not factors" $ x * y == t
{-# INLINABLE mkValidator #-}

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrap mkValidator ||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveVal :: IO ()
saveVal = writeValidatorToFile "./experiments/factoring.plutus" validator
