{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2
module TestUtils.TestContext.Helpers where
--------------------------------------------------------------------------------2
-- Non-IOG imports
import           Prelude                   as P

-- IOG imports
import qualified Ledger.Ada                as LedgerAda
import qualified Plutus.V1.Ledger.Interval as LedgerInterval
import qualified Plutus.V2.Ledger.Api      as LedgerApiV2
import qualified PlutusTx.AssocMap         as TxAssocMap
import           PlutusTx.Prelude          (divide, nub)
-- Project imports
import qualified Generic.Constants         as T
import qualified Generic.OnChainHelpers    as OnChainHelpers

------------------------------------------------------------------------------

someTxId :: LedgerApiV2.TxId
someTxId = LedgerApiV2.TxId "dd"

--------------------------------------------------------------------------------

createValidRange :: LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTimeRange
createValidRange date' = LedgerInterval.interval (date'  - LedgerApiV2.POSIXTime (LedgerApiV2.getPOSIXTime T.validTxTimeRange `divide` 2) + 1) (date' + LedgerApiV2.POSIXTime (LedgerApiV2.getPOSIXTime T.validTxTimeRange `divide` 2) -1)

createInValidRange :: LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTimeRange
createInValidRange date' = LedgerInterval.interval (date'  - LedgerApiV2.POSIXTime (LedgerApiV2.getPOSIXTime T.validTxTimeRange `divide` 2) + 1) (date' + (T.validTxTimeRange * 2))

---------------------------------------------------------------------------------

{- | Helper to easily build simple but useful script contexts.
-}

mkContext ::  LedgerApiV2.ScriptContext
mkContext = LedgerApiV2.ScriptContext txInfo purpose
    where
        txInfo :: LedgerApiV2.TxInfo
        txInfo =
            LedgerApiV2.TxInfo
                { LedgerApiV2.txInfoInputs = []
                , LedgerApiV2.txInfoReferenceInputs = []
                , LedgerApiV2.txInfoOutputs = []
                , txInfoFee = mempty
                , LedgerApiV2.txInfoMint = mempty
                , txInfoDCert = []
                , txInfoWdrl = LedgerApiV2.fromList []
                , LedgerApiV2.txInfoValidRange = LedgerApiV2.always
                , LedgerApiV2.txInfoSignatories = []
                , LedgerApiV2.txInfoRedeemers = LedgerApiV2.fromList []
                , txInfoData = TxAssocMap.fromList []
                , txInfoId = LedgerApiV2.TxId "ff"
                }

        purpose :: LedgerApiV2.ScriptPurpose
        purpose = LedgerApiV2.Spending $ LedgerApiV2.TxOutRef "" 1

--------------------------------------------------------------------------------
-- Setters
--------------------------------------------------------------------------------

setInputsRef :: [LedgerApiV2.TxOut] -> LedgerApiV2.ScriptContext -> LedgerApiV2.ScriptContext
setInputsRef inputs sc =
    sc
        { LedgerApiV2.scriptContextTxInfo =
            (LedgerApiV2.scriptContextTxInfo sc) {LedgerApiV2.txInfoReferenceInputs = txInInfos}
        }
    where
        txInInfos :: [LedgerApiV2.TxInInfo]
        txInInfos =
            [ LedgerApiV2.TxInInfo (LedgerApiV2.TxOutRef someTxId i) txIn
            | (i, txIn) <- zip [0 ..] inputs
            ]

setInputsAndAddRedeemers :: [(LedgerApiV2.TxOut, LedgerApiV2.Redeemer)] -> LedgerApiV2.ScriptContext -> LedgerApiV2.ScriptContext
setInputsAndAddRedeemers inputsWithRedeemer sc =
    sc
        { LedgerApiV2.scriptContextTxInfo =
            (LedgerApiV2.scriptContextTxInfo sc)
                { LedgerApiV2.txInfoInputs = txInInfos
                , LedgerApiV2.txInfoRedeemers = combinedRedeemers
                }
        }
    where
        txInInfos :: [LedgerApiV2.TxInInfo]
        txInInfos =
            [ LedgerApiV2.TxInInfo (LedgerApiV2.TxOutRef someTxId' i) txOut
            | (i, (txOut, _)) <- zip [0 ..] inputsWithRedeemer
            ]

        newRedeemersMap :: TxAssocMap.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        newRedeemersMap =
            TxAssocMap.fromList
                [ (LedgerApiV2.Spending (LedgerApiV2.TxOutRef someTxId' i), redeemer)
                | (i, (_, redeemer)) <- zip [0 ..] inputsWithRedeemer
                ]

         -- Helper function to check if the script purpose is spending
        isSpendingPurpose :: LedgerApiV2.ScriptPurpose -> Bool
        isSpendingPurpose (LedgerApiV2.Spending _) = True
        isSpendingPurpose _                        = False

        -- Get the existing redeemers from the context, filtering out any existing spending redeemers
        existingNonSpendingRedeemers :: TxAssocMap.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        existingNonSpendingRedeemers =
            TxAssocMap.fromList $ filter (\(purpose, _ ) ->  not (isSpendingPurpose purpose)) (TxAssocMap.toList existingRedeemers)

        existingRedeemers :: TxAssocMap.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        existingRedeemers = LedgerApiV2.txInfoRedeemers $ LedgerApiV2.scriptContextTxInfo sc

        combinedRedeemers :: TxAssocMap.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        combinedRedeemers = TxAssocMap.unionWith (\_ new -> new) existingNonSpendingRedeemers newRedeemersMap

        someTxId' = LedgerApiV2.TxId "0000000000000000000000000000000000000000000000000000000000000000"

setInputWithTxOufRef :: (LedgerApiV2.TxOut, LedgerApiV2.TxOutRef) -> LedgerApiV2.ScriptContext -> LedgerApiV2.ScriptContext
setInputWithTxOufRef (input, ref) sc =
    sc
        { LedgerApiV2.scriptContextTxInfo =
            (LedgerApiV2.scriptContextTxInfo sc) {LedgerApiV2.txInfoInputs = [LedgerApiV2.TxInInfo ref input]}
        }

addInputsWithTxOutRef :: [(LedgerApiV2.TxOut, LedgerApiV2.TxOutRef)] -> LedgerApiV2.ScriptContext -> LedgerApiV2.ScriptContext
addInputsWithTxOutRef newInputs sc =
    sc
        { LedgerApiV2.scriptContextTxInfo =
            (LedgerApiV2.scriptContextTxInfo sc)
                { LedgerApiV2.txInfoInputs = newTxInInfos ++ LedgerApiV2.txInfoInputs (LedgerApiV2.scriptContextTxInfo sc)
                }
        }
    where
        newTxInInfos = map (\(input, ref) -> LedgerApiV2.TxInInfo ref input) newInputs

-- setInfoRedeemers :: [LedgerApiV2.ScriptPurpose] -> [LedgerApiV2.Redeemer] -> LedgerApiV2.ScriptContext -> LedgerApiV2.ScriptContext
-- setInfoRedeemers purposes redeemers sc =
--     sc
--         { LedgerApiV2.scriptContextTxInfo =
--             (LedgerApiV2.scriptContextTxInfo sc)
--                 { LedgerApiV2.txInfoRedeemers = LedgerApiV2.fromList $ zip purposes redeemers
--                 }
--         }

setOutputs :: [LedgerApiV2.TxOut] -> LedgerApiV2.ScriptContext -> LedgerApiV2.ScriptContext
setOutputs outputs sc =
    sc
        { LedgerApiV2.scriptContextTxInfo =
            (LedgerApiV2.scriptContextTxInfo sc) {LedgerApiV2.txInfoOutputs = outputs}
        }

-- | Set the minting value of the transaction and add the redeemers, replacing all minting redeemers
setMintAndAddRedeemers :: [(LedgerApiV2.Value, LedgerApiV2.Redeemer)] -> LedgerApiV2.ScriptContext -> LedgerApiV2.ScriptContext
setMintAndAddRedeemers valsAndRedeemers sc =
    sc
        { LedgerApiV2.scriptContextTxInfo =
            (LedgerApiV2.scriptContextTxInfo sc)
                { LedgerApiV2.txInfoMint = combinedValue
                , LedgerApiV2.txInfoRedeemers = combinedRedeemers
                }
        }
    where
        -- Function to flatten the value and extract the single currency symbol
        extractSingleCurrencySymbol :: LedgerApiV2.Value -> LedgerApiV2.CurrencySymbol
        extractSingleCurrencySymbol val =
            let flattened = OnChainHelpers.flattenValueWithoutZeros val
                csList = nub [cs | (cs, _, _) <- flattened]
            in case csList of
                [cs] -> cs
                _    -> error $ "All elements must have the same currency symbol:" ++ show val

        -- Ensure each value has exactly one currency symbol and create redeemers for each value
        newRedeemersList :: [(LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)]
        newRedeemersList =
            [ (LedgerApiV2.Minting (extractSingleCurrencySymbol val), redeemer)
            | (val, redeemer) <- valsAndRedeemers
            ]

        -- Create a map from the list of redeemers
        newRedeemersMap :: TxAssocMap.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        newRedeemersMap = TxAssocMap.fromList newRedeemersList

        -- Helper function to check if the script purpose is minting
        isMintingPurpose :: LedgerApiV2.ScriptPurpose -> Bool
        isMintingPurpose (LedgerApiV2.Minting _) = True
        isMintingPurpose _                       = False

        -- Get the existing redeemers from the context, filtering out any existing minting redeemers
        existingNonMintingRedeemers :: TxAssocMap.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        existingNonMintingRedeemers =
            TxAssocMap.fromList $ filter (\(purpose, _ ) ->  not (isMintingPurpose purpose)) (TxAssocMap.toList existingRedeemers)

        -- Get the existing redeemers from the context
        existingRedeemers :: TxAssocMap.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        existingRedeemers = LedgerApiV2.txInfoRedeemers $ LedgerApiV2.scriptContextTxInfo sc

        -- Combine the new redeemers with the filtered existing ones
        combinedRedeemers :: TxAssocMap.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        combinedRedeemers = TxAssocMap.unionWith (\_ new -> new) existingNonMintingRedeemers newRedeemersMap

        -- Combine all values into one
        combinedValue :: LedgerApiV2.Value
        combinedValue = LedgerAda.lovelaceValueOf 0 <> mconcat (map fst valsAndRedeemers)

setValidyRange :: LedgerApiV2.POSIXTimeRange -> LedgerApiV2.ScriptContext -> LedgerApiV2.ScriptContext
setValidyRange range sc@LedgerApiV2.ScriptContext {..} =
    sc {LedgerApiV2.scriptContextTxInfo = scriptContextTxInfo {LedgerApiV2.txInfoValidRange = range}}

setSignatories :: [LedgerApiV2.PubKeyHash] -> LedgerApiV2.ScriptContext -> LedgerApiV2.ScriptContext
setSignatories signers sc =
    sc
        { LedgerApiV2.scriptContextTxInfo =
            (LedgerApiV2.scriptContextTxInfo sc) {LedgerApiV2.txInfoSignatories = signers}
        }

setSpendPurpose :: Integer -> LedgerApiV2.ScriptContext -> LedgerApiV2.ScriptContext
setSpendPurpose consumeInputIndex ctx =
    let
        !txInfoInputs = LedgerApiV2.txInfoInputs $ LedgerApiV2.scriptContextTxInfo ctx
        !txInInfo = txInfoInputs!!fromIntegral consumeInputIndex
        purpose :: LedgerApiV2.ScriptPurpose
        purpose = LedgerApiV2.Spending $ LedgerApiV2.txInInfoOutRef txInInfo
    in
        ctx { LedgerApiV2.scriptContextPurpose = purpose }

--------------------------------------------------------------------------------
