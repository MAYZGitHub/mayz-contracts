--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

{- |
Module      : TestUtils.Contracts.TxContext.SwapOffer
Description : Mock Data and Auxiliary Functions for testing the SwapOffer.
-}
module TestUtils.Contracts.TxContext.SwapOffer where

--------------------------------------------------------------------------------3

-- Non-IOG imports

-- IOG imports
import qualified Plutus.V2.Ledger.Api            as LedgerApiV2
import           PlutusTx.Prelude

-- Project imports

import qualified Generic.OnChainHelpers          as OnChainHelpers
import qualified Ledger.Value                    as LedgerValue
import qualified Protocol.Constants              as T
import qualified Protocol.SwapOffer.OnChain      as SwapOffer
import qualified Protocol.SwapOffer.Types        as SwapOfferT
import           TestUtils.Contracts.InitialData
import           TestUtils.Helpers
import           TestUtils.TestContext.Helpers
import           TestUtils.Types
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------
-- SwapOffer Contract
--------------------------------------------------------------------------------

swapOffer_Create_TxContext :: TestParams -> LedgerApiV2.ScriptContext
swapOffer_Create_TxContext tp =
    mkContext
        |> setInputsRef [protocol_UTxO_MockData tp, fund_UTxO_MockData tp]
        |> setOutputs [swapOffer_UTxO_MockData tp]
        |> setMintAndAddRedeemers
            [
                ( LedgerApiV2.singleton
                    (tpSwapOfferPolicyID_CS tp)
                    T.swapOfferID_TN
                    1
                , SwapOfferT.mkMintIDRedeemer
                )
            ]
        |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------

swapOffer_Delete_TxContext :: TestParams -> LedgerApiV2.ScriptContext
swapOffer_Delete_TxContext tp =
    mkContext
        |> setInputsRef
            [ protocol_UTxO_MockData tp
            , uTxOForValidatorAsReference tp (tpSwapOfferValidator tp)
            , uTxOForMintingAsReference tp (tpSwapOfferPolicyID tp)
            ]
        |> setInputsAndAddRedeemers
            [(swapOffer_UTxO_MockData tp, SwapOfferT.mkDeleteRedeemer)]
        |> setMintAndAddRedeemers
            [
                ( LedgerApiV2.singleton (tpSwapOfferPolicyID_CS tp) T.swapOfferID_TN $
                    negate 1
                , SwapOfferT.mkBurnIDRedeemer
                )
            ]
        |> setSignatories [tpSwapOfferAdmin tp]
        |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------

swapOffer_UpdateStatus_TxContext :: TestParams -> Integer -> LedgerApiV2.ScriptContext
swapOffer_UpdateStatus_TxContext tp newStatus =
    let
        input_SwapOffer_UTxO = swapOffer_UTxO_MockData tp
        -----------------
        input_SwapOffer_Datum = SwapOfferT.getSwapOffer_DatumType_From_UTxO input_SwapOffer_UTxO
        -----------------
        output_SwapOffer_Datum = SwapOffer.mkUpdated_SwapOffer_Datum_With_StatusChanged
                input_SwapOffer_Datum
                newStatus
        -----------------
        output_SwapOffer_UTxO = input_SwapOffer_UTxO
            { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $
                    SwapOfferT.mkDatum output_SwapOffer_Datum
            }
        -----------------
    in
        mkContext
            |> setInputsRef [fund_UTxO_MockData tp]
            |> setInputsAndAddRedeemers
                [
                    ( input_SwapOffer_UTxO
                    , SwapOfferT.mkUpdateStatusRedeemer newStatus
                    )
                ]
            |> setOutputs [output_SwapOffer_UTxO]
            |> setSignatories [tpSwapOfferAdmin tp]
            |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------

swapOffer_UpdateAskedCommissionRate_TxContext :: TestParams -> Integer -> LedgerApiV2.ScriptContext
swapOffer_UpdateAskedCommissionRate_TxContext tp newCommissionRate =
    let
        input_SwapOffer_UTxO = swapOffer_UTxO_MockData tp
        -----------------
        input_SwapOffer_Datum = SwapOfferT.getSwapOffer_DatumType_From_UTxO input_SwapOffer_UTxO
        -----------------
        output_SwapOffer_Datum = SwapOffer.mkUpdated_SwapOffer_Datum_With_CommissionChanged
                input_SwapOffer_Datum
                newCommissionRate
        -----------------
        output_SwapOffer_UTxO = input_SwapOffer_UTxO
            { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $
                    SwapOfferT.mkDatum output_SwapOffer_Datum
            }
        -----------------
    in
        mkContext
            |> setInputsRef [protocol_UTxO_MockData tp, fund_UTxO_MockData tp]
            |> setInputsAndAddRedeemers
                [
                    ( input_SwapOffer_UTxO
                    , SwapOfferT.mkUpdateAskedCommissionRateRedeemer newCommissionRate
                    )
                ]
            |> setOutputs [output_SwapOffer_UTxO]
            |> setSignatories [tpSwapOfferAdmin tp]
            |> setValidyRange (createValidRange (tpTransactionDate tp))
--------------------------------------------------------------------------------

swapOffer_UpdateSellRestrictions_TxContext :: TestParams -> Integer -> Integer -> LedgerApiV2.ScriptContext
swapOffer_UpdateSellRestrictions_TxContext tp newAllowSellFT newAllowSellADA =
    let
        input_SwapOffer_UTxO = swapOffer_UTxO_MockData tp
        -----------------
        input_SwapOffer_Datum = SwapOfferT.getSwapOffer_DatumType_From_UTxO input_SwapOffer_UTxO
        -----------------
        output_SwapOffer_Datum = SwapOffer.mkUpdated_SwapOffer_Datum_With_RestrictionsChanged
                input_SwapOffer_Datum
                newAllowSellFT newAllowSellADA
        -----------------
        output_SwapOffer_UTxO = input_SwapOffer_UTxO
            { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $
                    SwapOfferT.mkDatum output_SwapOffer_Datum
            }
        -----------------
    in
        mkContext
            |> setInputsRef [fund_UTxO_MockData tp]
            |> setInputsAndAddRedeemers
                [
                    ( input_SwapOffer_UTxO
                    , SwapOfferT.mkUpdateSellRestrictionsRedeemer newAllowSellFT newAllowSellADA
                    )
                ]
            |> setOutputs [output_SwapOffer_UTxO]
            |> setSignatories [tpSwapOfferAdmin tp]
            |> setValidyRange (createValidRange (tpTransactionDate tp))
--------------------------------------------------------------------------------

swapOffer_UpdateMinADA_TxContext :: TestParams -> Integer -> LedgerApiV2.ScriptContext
swapOffer_UpdateMinADA_TxContext tp newMinADA =
    let
        input_SwapOffer_UTxO = swapOffer_UTxO_MockData tp
        -----------------
        input_SwapOffer_Datum = SwapOfferT.getSwapOffer_DatumType_From_UTxO input_SwapOffer_UTxO
        -----------------
        input_SwapOffer_Value = LedgerApiV2.txOutValue input_SwapOffer_UTxO
        -----------------
        output_SwapOffer_Datum = SwapOffer.mkUpdated_SwapOffer_Datum_With_MinADAChanged
                input_SwapOffer_Datum
                newMinADA
        -----------------
        newAmount_ADA = SwapOfferT.sodAmount_ADA_Available output_SwapOffer_Datum + SwapOfferT.sodMinADA output_SwapOffer_Datum
        -----------------
        output_SwapOffer_UTxO = input_SwapOffer_UTxO
            { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $
                    SwapOfferT.mkDatum output_SwapOffer_Datum
            , LedgerApiV2.txOutValue =
                changeValue_Amount
                    input_SwapOffer_Value
                    OnChainHelpers.adaAssetClass
                    newAmount_ADA
            }
        -----------------
    in
        mkContext
            |> setInputsRef [fund_UTxO_MockData tp]
            |> setInputsAndAddRedeemers [(input_SwapOffer_UTxO, SwapOfferT.mkUpdateMinADARedeemer)]
            |> setOutputs [output_SwapOffer_UTxO]
            |> setSignatories [tpSwapOfferAdmin tp]
            |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------

swapOffer_Deposit_TxContext :: TestParams -> Integer ->  Integer -> LedgerApiV2.ScriptContext
swapOffer_Deposit_TxContext tp !newDeposit_FT !newDeposit_ADA =
    let
        input_SwapOffer_UTxO = swapOffer_UTxO_MockData tp
        -----------------
        input_SwapOffer_Datum = SwapOfferT.getSwapOffer_DatumType_From_UTxO input_SwapOffer_UTxO
        input_SwapOffer_Value = LedgerApiV2.txOutValue input_SwapOffer_UTxO
        -----------------
        output_SwapOffer_Datum = SwapOffer.mkUpdated_SwapOffer_Datum_With_Deposit
                input_SwapOffer_Datum
                newDeposit_FT newDeposit_ADA
        -----------------
        newAmount_ADA = SwapOfferT.sodAmount_ADA_Available output_SwapOffer_Datum + SwapOfferT.sodMinADA output_SwapOffer_Datum
        newAmount_FT = SwapOfferT.sodAmount_FT_Available output_SwapOffer_Datum
        -----------------
        adjustedValue_ADA =
            changeValue_Amount
                input_SwapOffer_Value
                OnChainHelpers.adaAssetClass
                newAmount_ADA
        adjustedValue_FT_And_ADA =
            changeValue_Amount
                adjustedValue_ADA
                (LedgerValue.assetClass (tpFundPolicy_CS tp) (tpFundFT_TN tp))
                newAmount_FT
        -----------------
        output_SwapOffer_UTxO = input_SwapOffer_UTxO
            { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $
                    SwapOfferT.mkDatum output_SwapOffer_Datum
            , LedgerApiV2.txOutValue = adjustedValue_FT_And_ADA
            }
        -----------------
    in
        mkContext
            |> setInputsRef [fund_UTxO_MockData tp]
            |> setInputsAndAddRedeemers [(input_SwapOffer_UTxO, SwapOfferT.mkDepositRedeemer newDeposit_FT newDeposit_ADA)]
            |> setOutputs [output_SwapOffer_UTxO]
            |> setSignatories [tpSwapOfferAdmin tp]
            |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------

swapOffer_Withdraw_TxContext :: TestParams -> Integer ->  Integer -> LedgerApiV2.ScriptContext
swapOffer_Withdraw_TxContext tp !newWithdraw_FT !newWithdraw_ADA =
    let
        input_SwapOffer_UTxO = swapOffer_UTxO_MockData tp
        -----------------
        input_SwapOffer_Datum = SwapOfferT.getSwapOffer_DatumType_From_UTxO input_SwapOffer_UTxO
        input_SwapOffer_Value = LedgerApiV2.txOutValue input_SwapOffer_UTxO
        -----------------
        output_SwapOffer_Datum = SwapOffer.mkUpdated_SwapOffer_Datum_With_Withdraw
                input_SwapOffer_Datum
                newWithdraw_FT newWithdraw_ADA
        -----------------
        newAmount_ADA = SwapOfferT.sodAmount_ADA_Available output_SwapOffer_Datum + SwapOfferT.sodMinADA output_SwapOffer_Datum
        newAmount_FT = SwapOfferT.sodAmount_FT_Available output_SwapOffer_Datum
        -----------------
        adjustedValue_ADA =
            changeValue_Amount
                input_SwapOffer_Value
                OnChainHelpers.adaAssetClass
                newAmount_ADA
        adjustedValue_FT_And_ADA =
            changeValue_Amount
                adjustedValue_ADA
                (LedgerValue.assetClass (tpFundPolicy_CS tp) (tpFundFT_TN tp))
                newAmount_FT
        -----------------
        output_SwapOffer_UTxO = input_SwapOffer_UTxO
            { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $
                    SwapOfferT.mkDatum output_SwapOffer_Datum
            , LedgerApiV2.txOutValue = adjustedValue_FT_And_ADA
            }
        -----------------
    in
        mkContext
            |> setInputsRef [fund_UTxO_MockData tp]
            |> setInputsAndAddRedeemers [(input_SwapOffer_UTxO, SwapOfferT.mkWithdrawRedeemer newWithdraw_FT newWithdraw_ADA)]
            |> setOutputs [output_SwapOffer_UTxO]
            |> setSignatories [tpSwapOfferAdmin tp]
            |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------

swapOffer_SwapFTxADA_TxContext :: TestParams -> Integer -> Integer -> Integer -> Integer -> Integer -> LedgerApiV2.ScriptContext
swapOffer_SwapFTxADA_TxContext tp !commission_InBPx1e3 !token_FT_Price1xe6 !amount_FT !amount_ADA !commission_ADA =
    let
        --------
        input_SwapOffer_UTxO' = swapOffer_UTxO_MockData tp
        -----------------
        input_SwapOffer_Datum' = SwapOfferT.getSwapOffer_DatumType_From_UTxO input_SwapOffer_UTxO'
        -----------------
        input_SwapOffer_Datum = input_SwapOffer_Datum' {
                SwapOfferT.sodAskedCommission_InBPx1e3 = commission_InBPx1e3
            }
        -----------------
        input_SwapOffer_Value = LedgerApiV2.txOutValue input_SwapOffer_UTxO'
        -----------------
        input_SwapOffer_UTxO = input_SwapOffer_UTxO' {
                LedgerApiV2.txOutDatum =
                    LedgerApiV2.OutputDatum $
                        SwapOfferT.mkDatum input_SwapOffer_Datum
            }
        -----------------
        output_SwapOffer_Datum = SwapOffer.mkUpdated_SwapOffer_Datum_With_SwapFTxADA
                input_SwapOffer_Datum
                amount_FT amount_ADA commission_ADA
        -----------------
        oracleData = mkOracleData tp token_FT_Price1xe6 (tpTransactionDate tp)
        oracleDataSignature = mkOracleDataSignature tp oracleData
        -----------------
        newAmount_ADA = SwapOfferT.sodAmount_ADA_Available output_SwapOffer_Datum + SwapOfferT.sodMinADA output_SwapOffer_Datum
        newAmount_FT = SwapOfferT.sodAmount_FT_Available output_SwapOffer_Datum
        -----------------
        adjustedValue_ADA =
            changeValue_Amount
                input_SwapOffer_Value
                OnChainHelpers.adaAssetClass
                newAmount_ADA
        adjustedValue_FT_And_ADA =
            changeValue_Amount
                adjustedValue_ADA
                (LedgerValue.assetClass (tpFundPolicy_CS tp) (tpFundFT_TN tp))
                newAmount_FT
        -----------------
        output_SwapOffer_UTxO = input_SwapOffer_UTxO
            { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $
                    SwapOfferT.mkDatum output_SwapOffer_Datum
            , LedgerApiV2.txOutValue = adjustedValue_FT_And_ADA
            }
        -----------------
    in
        mkContext
            |> setInputsRef [protocol_UTxO_MockData tp, fund_UTxO_MockData tp]
            |> setInputsAndAddRedeemers [(input_SwapOffer_UTxO, SwapOfferT.mkSwapFTxADARedeemer amount_FT amount_ADA commission_ADA oracleData oracleDataSignature)]
            |> setOutputs [output_SwapOffer_UTxO]
            |> setSignatories (tpFundAdmins tp)
            |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------

swapOffer_SwapADAxFT_TxContext :: TestParams -> Integer -> Integer -> Integer -> Integer -> Integer -> LedgerApiV2.ScriptContext
swapOffer_SwapADAxFT_TxContext tp !commission_InBPx1e3 !token_FT_Price1xe6 !amount_ADA !amount_FT !commission_FT =
    let
        --------
        input_SwapOffer_UTxO' = swapOffer_UTxO_MockData tp
        -----------------
        input_SwapOffer_Datum' = SwapOfferT.getSwapOffer_DatumType_From_UTxO input_SwapOffer_UTxO'
        -----------------
        input_SwapOffer_Datum = input_SwapOffer_Datum' {
                SwapOfferT.sodAskedCommission_InBPx1e3 = commission_InBPx1e3
            }
        -----------------
        input_SwapOffer_Value = LedgerApiV2.txOutValue input_SwapOffer_UTxO'
        -----------------
        input_SwapOffer_UTxO = input_SwapOffer_UTxO' {
                LedgerApiV2.txOutDatum =
                    LedgerApiV2.OutputDatum $
                        SwapOfferT.mkDatum input_SwapOffer_Datum
            }
        -----------------
        output_SwapOffer_Datum = SwapOffer.mkUpdated_SwapOffer_Datum_With_SwapADAxFT
                input_SwapOffer_Datum
                amount_ADA amount_FT commission_FT
        -----------------
        oracleData = mkOracleData tp token_FT_Price1xe6 (tpTransactionDate tp)
        oracleDataSignature = mkOracleDataSignature tp oracleData
        -----------------
        newAmount_ADA = SwapOfferT.sodAmount_ADA_Available output_SwapOffer_Datum + SwapOfferT.sodMinADA output_SwapOffer_Datum
        newAmount_FT = SwapOfferT.sodAmount_FT_Available output_SwapOffer_Datum
        -----------------
        adjustedValue_ADA =
            changeValue_Amount
                input_SwapOffer_Value
                OnChainHelpers.adaAssetClass
                newAmount_ADA
        adjustedValue_FT_And_ADA =
            changeValue_Amount
                adjustedValue_ADA
                (LedgerValue.assetClass (tpFundPolicy_CS tp) (tpFundFT_TN tp))
                newAmount_FT
        -----------------
        output_SwapOffer_UTxO = input_SwapOffer_UTxO
            { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $
                    SwapOfferT.mkDatum output_SwapOffer_Datum
            , LedgerApiV2.txOutValue = adjustedValue_FT_And_ADA
            }
        -----------------
    in
        mkContext
            |> setInputsRef [protocol_UTxO_MockData tp, fund_UTxO_MockData tp]
            |> setInputsAndAddRedeemers [(input_SwapOffer_UTxO, SwapOfferT.mkSwapADAxFTRedeemer amount_ADA amount_FT commission_FT oracleData oracleDataSignature)]
            |> setOutputs [output_SwapOffer_UTxO]
            |> setSignatories (tpFundAdmins tp)
            |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------
