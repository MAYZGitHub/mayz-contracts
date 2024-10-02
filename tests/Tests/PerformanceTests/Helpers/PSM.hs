{-# LANGUAGE DataKinds #-}
--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3
module Helpers.PSM where

------------------------------------------------------------------------------------------
-- External Imports
------------------------------------------------------------------------------------------

import qualified Control.Monad        as ControlMonad
import qualified Plutus.Model         as PlutusSimpleModel
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import           Prelude

------------------------------------------------------------------------------------------
-- Internal Imports
------------------------------------------------------------------------------------------
import           Helpers.Validator
------------------------------------------------------------------------------------------

-- Validator's script
valScript :: PlutusSimpleModel.TypedValidator datum redeemer
valScript = PlutusSimpleModel.TypedValidator $ PlutusSimpleModel.toV2 validator

-- Set many users at once
setupUsers :: PlutusSimpleModel.Run [LedgerApiV2.PubKeyHash]
setupUsers = ControlMonad.replicateM 2 $ PlutusSimpleModel.newUser $ PlutusSimpleModel.ada (PlutusSimpleModel.Lovelace 1000)

-- Create transaction that spends "spUTXO" to lock "val" in "valScript"
lockingTx :: PlutusSimpleModel.UserSpend -> CustomDatum -> LedgerApiV2.Value -> PlutusSimpleModel.Tx
lockingTx spUTXO datum val =
    mconcat
        [ PlutusSimpleModel.userSpend spUTXO
        , PlutusSimpleModel.payToScript valScript (PlutusSimpleModel.HashDatum datum) val
        ]

-- Create transaction that spends "refUTXO" to unlock "val" from the "valScript" validator
consumingTx :: CustomDatum -> CustomRedeemer -> LedgerApiV2.PubKeyHash -> LedgerApiV2.TxOutRef -> LedgerApiV2.Value -> PlutusSimpleModel.Tx
consumingTx datum redeemer usrPKH refUTXO val =
    mconcat
        [ PlutusSimpleModel.spendScript valScript refUTXO redeemer datum
        , PlutusSimpleModel.payToKey usrPKH val
        ]

waitBeforeConsumingTx :: LedgerApiV2.POSIXTime
waitBeforeConsumingTx = 1000

------------------------------------------------------------------------------------------

-- Function to test if both creating and consuming script UTxOs works properly
testScript :: CustomDatum -> CustomRedeemer -> PlutusSimpleModel.Run ()
testScript datum redeemer = do
    -- SETUP USERS
    [u1, u2] <- setupUsers
    -- USER 1 LOCKS 100 Lovelaces ("val") IN VALIDATOR
    let
        val = PlutusSimpleModel.adaValue 100 -- Define value to be transfered
    spUTXO <- PlutusSimpleModel.spend u1 val -- Get user's UTXO that we should spend
    PlutusSimpleModel.submitTx u1 $ lockingTx spUTXO datum val -- User 1 submits "lockingTx" transaction
    -- WAIT FOR A BIT
    PlutusSimpleModel.waitUntil waitBeforeConsumingTx

    -- USER 2 TAKES "val" FROM VALIDATOR
    utxos <- PlutusSimpleModel.utxoAt valScript -- Query blockchain to get all UTxOs at script
    let
        (ref, out) = case utxos of
            [(ref', out')] -> (ref', out')
             -- We know there is only one UTXO (the one we created before)
            _              -> error "Expected exactly one UTXO"
    ct <- PlutusSimpleModel.currentTimeRad 100 -- Create time interval with equal radius around current time
    tx <- PlutusSimpleModel.validateIn ct $ consumingTx datum redeemer u2 ref (LedgerApiV2.txOutValue out) -- Build Tx
    PlutusSimpleModel.submitTx u2 tx -- User 2 submits "consumingTx" transaction
    -- CHECK THAT FINAL BALANCES MATCH EXPECTED BALANCES
    [v1, v2] <- ControlMonad.mapM PlutusSimpleModel.valueAt [u1, u2] -- Get final balances of both users
    ControlMonad.unless (v1 == PlutusSimpleModel.adaValue 900 && v2 == PlutusSimpleModel.adaValue 1100) $ -- Check if final balances match expected balances
        PlutusSimpleModel.logError "Final balances are incorrect"

------------------------------------------------------------------------------------------
