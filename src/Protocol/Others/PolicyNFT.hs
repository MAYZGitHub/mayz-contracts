{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}

--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

module Protocol.Others.PolicyNFT where

--------------------------------------------------------------------------------2
-- External Imports
--------------------------------------------------------------------------------2

import qualified Ledger.Value              as LedgerValue
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api      as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import qualified PlutusTx
import           PlutusTx.Prelude

--------------------------------------------------------------------------------2
-- Internal Imports
--------------------------------------------------------------------------------2

--------------------------------------------------------------------------------2
-- Module
--------------------------------------------------------------------------------2

{-# INLINEABLE mkPolicyNFT #-}
mkPolicyNFT :: LedgerApiV2.TxOutRef -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkPolicyNFT oref _ !ctxRaw =
    if traceIfFalse "UTxO not consumed" hasInputUTxO
        && traceIfFalse "Wrong amount minted" checkMintingAmount
        then ()
        else error ()
    where
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        ------------------
        info :: LedgerContextsV2.TxInfo
        info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        hasInputUTxO :: Bool
        hasInputUTxO = any (\i -> LedgerApiV2.txInInfoOutRef i == oref) $ LedgerApiV2.txInfoInputs info
        ------------------
        !cs = LedgerContextsV2.ownCurrencySymbol ctx
        ------------------
        checkMintingAmount :: Bool
        checkMintingAmount = allOnes (LedgerValue.flattenValue (LedgerApiV2.txInfoMint info))
        -- case LedgerValueV1.flattenValue (LedgerApiV1.txInfoMint info) of
        --     [(cs, tn', amt)] -> tn' == tn &&
        --                        amt == 1
        --     _               -> False
        ------------------
        allOnes :: [(LedgerValue.CurrencySymbol, b, Integer)] -> Bool
        allOnes lst = all (\(cs', _, amt) -> cs' == cs && amt == 1) lst

--------------------------------------------------------------------------------2

{-# INLINEABLE policyNFT #-}
policyNFT :: LedgerApiV2.TxOutRef -> LedgerApiV2.MintingPolicy
policyNFT oref =
    Plutonomy.optimizeUPLC $
        Plutonomy.mintingPolicyToPlutus $
            Plutonomy.mkMintingPolicyScript $
                $$(PlutusTx.compile [||mkPolicyNFT||])
                    `PlutusTx.applyCode` PlutusTx.liftCode oref

--------------------------------------------------------------------------------2
