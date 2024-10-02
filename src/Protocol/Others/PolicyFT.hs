{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}

--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

module Protocol.Others.PolicyFT where

--------------------------------------------------------------------------------2
-- External Imports
--------------------------------------------------------------------------------2

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api       as LedgerApiV2
import qualified PlutusTx
import qualified PlutusTx.Builtins.Internal as TxBuiltinsInternal (BuiltinInteger)
import           PlutusTx.Prelude           (BuiltinData, Integer, error, ($), (>))

--------------------------------------------------------------------------------2
-- Internal Imports
--------------------------------------------------------------------------------2

--------------------------------------------------------------------------------2
-- Module
--------------------------------------------------------------------------------2

{-# INLINEABLE mkPolicyFT #-}
mkPolicyFT :: TxBuiltinsInternal.BuiltinInteger -> BuiltinData -> BuiltinData -> ()
mkPolicyFT numero _ _ =
    if numero > 0 then () else error ()

--------------------------------------------------------------------------------2

{-# INLINEABLE policyFT #-}
policyFT :: Integer -> LedgerApiV2.MintingPolicy
policyFT numero =
    Plutonomy.optimizeUPLC $
        Plutonomy.mintingPolicyToPlutus $
            Plutonomy.mkMintingPolicyScript $
                $$(PlutusTx.compile [||mkPolicyFT||])
                    `PlutusTx.applyCode` PlutusTx.liftCode numero

--------------------------------------------------------------------------------2
