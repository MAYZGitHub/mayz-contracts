--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

{- |
Module      : TestUtils.Contracts.TxContext.SellOffer
Description : Mock Data and Auxiliary Functions for testing the SellOffer.
-}
module TestUtils.Contracts.TxContext.SellOffer where

--------------------------------------------------------------------------------3

-- Non-IOG imports

-- IOG imports
import qualified Plutus.V2.Ledger.Api                 as LedgerApiV2
import           PlutusTx.Prelude

-- Project imports

import qualified Protocol.Constants                   as T
import qualified Protocol.SellOffer.Types             as SellOfferT
import           TestUtils.Contracts.InitialData
import           TestUtils.Helpers
import           TestUtils.TestContext.Helpers
import           TestUtils.Types
import           TestUtils.TypesMAYZ
import qualified Generic.OnChainHelpers as OnChainHelpers
import qualified Protocol.SellOffer.OnChain as SellOffer
import qualified Ledger.Value as LedgerValue

--------------------------------------------------------------------------------
-- SellOffer Contract
--------------------------------------------------------------------------------

sellOffer_Create_TxContext :: TestParams -> LedgerApiV2.ScriptContext
sellOffer_Create_TxContext tp =
    mkContext
        |> setInputsRef [protocol_UTxO_MockData tp, fund_UTxO_MockData tp]
        |> setOutputs [sellOffer_UTxO_MockData tp]
        |> setMintAndAddRedeemers
            [
                ( LedgerApiV2.singleton
                    (tpSellOfferPolicyID_CS tp)
                    T.sellOfferID_TN
                    1
                , SellOfferT.mkMintIDRedeemer
                )
            ]
        |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------

sellOffer_Delete_TxContext :: TestParams -> LedgerApiV2.ScriptContext
sellOffer_Delete_TxContext tp =
    mkContext
        |> setInputsRef
            [ protocol_UTxO_MockData tp
            , uTxOForValidatorAsReference tp (tpSellOfferValidator tp)
            , uTxOForMintingAsReference tp (tpSellOfferPolicyID tp)
            ]
        |> setInputsAndAddRedeemers
            [(sellOffer_UTxO_MockData tp, SellOfferT.mkDeleteRedeemer)]
        |> setMintAndAddRedeemers
            [
                ( LedgerApiV2.singleton (tpSellOfferPolicyID_CS tp) T.sellOfferID_TN $
                    negate 1
                , SellOfferT.mkBurnIDRedeemer
                )
            ]
        |> setSignatories [tpSellOfferAdmin tp]
        |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------

sellOffer_UpdateStatus_TxContext :: TestParams -> Integer -> LedgerApiV2.ScriptContext
sellOffer_UpdateStatus_TxContext tp newStatus =
    let
        input_SellOffer_UTxO = sellOffer_UTxO_MockData tp
        -----------------
        input_SellOffer_Datum = SellOfferT.getSellOffer_DatumType_From_UTxO input_SellOffer_UTxO
        -----------------
        output_SellOffer_Datum = SellOffer.mkUpdated_SellOffer_Datum_With_StatusChanged
                input_SellOffer_Datum
                newStatus
        -----------------
        output_SellOffer_UTxO = input_SellOffer_UTxO
            { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $
                    SellOfferT.mkDatum output_SellOffer_Datum
            }
        -----------------
    in
        mkContext
            |> setInputsRef [fund_UTxO_MockData tp]
            |> setInputsAndAddRedeemers
                [
                    ( input_SellOffer_UTxO
                    , SellOfferT.mkUpdateStatusRedeemer newStatus
                    )
                ]
            |> setOutputs [output_SellOffer_UTxO]
            |> setSignatories [tpSellOfferAdmin tp]
            |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------

sellOffer_UpdateAskedCommissionRate_TxContext :: TestParams -> Integer -> LedgerApiV2.ScriptContext
sellOffer_UpdateAskedCommissionRate_TxContext tp newCommissionRate =
    let
        input_SellOffer_UTxO = sellOffer_UTxO_MockData tp
        -----------------
        input_SellOffer_Datum = SellOfferT.getSellOffer_DatumType_From_UTxO input_SellOffer_UTxO
        -----------------
        output_SellOffer_Datum = SellOffer.mkUpdated_SellOffer_Datum_With_CommissionChanged
                input_SellOffer_Datum
                newCommissionRate
        -----------------
        output_SellOffer_UTxO = input_SellOffer_UTxO
            { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $
                    SellOfferT.mkDatum output_SellOffer_Datum
            }
        -----------------
    in
        mkContext
            |> setInputsRef [protocol_UTxO_MockData tp, fund_UTxO_MockData tp]
            |> setInputsAndAddRedeemers
                [
                    ( input_SellOffer_UTxO
                    , SellOfferT.mkUpdateAskedCommissionRateRedeemer newCommissionRate
                    )
                ]
            |> setOutputs [output_SellOffer_UTxO]
            |> setSignatories [tpSellOfferAdmin tp]
            |> setValidyRange (createValidRange (tpTransactionDate tp))
--------------------------------------------------------------------------------

sellOffer_UpdateSellRestrictions_TxContext :: TestParams -> Integer -> Integer -> LedgerApiV2.ScriptContext
sellOffer_UpdateSellRestrictions_TxContext tp newAllowSellFT newAllowSellADA =
    let
        input_SellOffer_UTxO = sellOffer_UTxO_MockData tp
        -----------------
        input_SellOffer_Datum = SellOfferT.getSellOffer_DatumType_From_UTxO input_SellOffer_UTxO
        -----------------
        output_SellOffer_Datum = SellOffer.mkUpdated_SellOffer_Datum_With_RestrictionsChanged 
                input_SellOffer_Datum
                newAllowSellFT newAllowSellADA
        -----------------
        output_SellOffer_UTxO = input_SellOffer_UTxO
            { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $
                    SellOfferT.mkDatum output_SellOffer_Datum
            }
        -----------------
    in
        mkContext
            |> setInputsRef [fund_UTxO_MockData tp]
            |> setInputsAndAddRedeemers
                [
                    ( input_SellOffer_UTxO
                    , SellOfferT.mkUpdateSellRestrictionsRedeemer newAllowSellFT newAllowSellADA
                    )
                ]
            |> setOutputs [output_SellOffer_UTxO]
            |> setSignatories [tpSellOfferAdmin tp]
            |> setValidyRange (createValidRange (tpTransactionDate tp))
--------------------------------------------------------------------------------

sellOffer_UpdateMinADA_TxContext :: TestParams -> Integer -> LedgerApiV2.ScriptContext
sellOffer_UpdateMinADA_TxContext tp newMinADA =
    let
        input_SellOffer_UTxO = sellOffer_UTxO_MockData tp
        -----------------
        input_SellOffer_Datum = SellOfferT.getSellOffer_DatumType_From_UTxO input_SellOffer_UTxO
        -----------------
        input_SellOffer_Value = LedgerApiV2.txOutValue input_SellOffer_UTxO
        -----------------
        output_SellOffer_Datum = SellOffer.mkUpdated_SellOffer_Datum_With_MinADAChanged
                input_SellOffer_Datum
                newMinADA
        -----------------
        newAmount_ADA = SellOfferT.sodAmount_ADA_Available output_SellOffer_Datum + SellOfferT.sodMinADA output_SellOffer_Datum
        -----------------
        output_SellOffer_UTxO = input_SellOffer_UTxO
            { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $
                    SellOfferT.mkDatum output_SellOffer_Datum
            , LedgerApiV2.txOutValue =
                changeValue_Amount
                    input_SellOffer_Value
                    OnChainHelpers.adaAssetClass
                    newAmount_ADA
            }
        -----------------
    in
        mkContext
            |> setInputsRef [fund_UTxO_MockData tp]
            |> setInputsAndAddRedeemers [(input_SellOffer_UTxO, SellOfferT.mkUpdateMinADARedeemer)]
            |> setOutputs [output_SellOffer_UTxO]
            |> setSignatories [tpSellOfferAdmin tp]
            |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------

sellOffer_Deposit_TxContext :: TestParams -> Integer ->  Integer -> LedgerApiV2.ScriptContext
sellOffer_Deposit_TxContext tp !newDeposit_FT !newDeposit_ADA =
    let
        input_SellOffer_UTxO = sellOffer_UTxO_MockData tp
        -----------------
        input_SellOffer_Datum = SellOfferT.getSellOffer_DatumType_From_UTxO input_SellOffer_UTxO
        input_SellOffer_Value = LedgerApiV2.txOutValue input_SellOffer_UTxO
        -----------------
        output_SellOffer_Datum = SellOffer.mkUpdated_SellOffer_Datum_With_Deposit
                input_SellOffer_Datum
                newDeposit_FT newDeposit_ADA
        -----------------
        newAmount_ADA = SellOfferT.sodAmount_ADA_Available output_SellOffer_Datum + SellOfferT.sodMinADA output_SellOffer_Datum
        newAmount_FT = SellOfferT.sodAmount_FT_Available output_SellOffer_Datum
        -----------------
        adjustedValue_ADA =
            changeValue_Amount
                input_SellOffer_Value
                OnChainHelpers.adaAssetClass
                newAmount_ADA
        adjustedValue_FT_And_ADA =
            changeValue_Amount
                adjustedValue_ADA
                (LedgerValue.assetClass (tpFundPolicy_CS tp) (tpFundFT_TN tp))
                newAmount_FT
        -----------------
        output_SellOffer_UTxO = input_SellOffer_UTxO
            { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $
                    SellOfferT.mkDatum output_SellOffer_Datum
            , LedgerApiV2.txOutValue = adjustedValue_FT_And_ADA
            }
        -----------------
    in
        mkContext
            |> setInputsRef [fund_UTxO_MockData tp]
            |> setInputsAndAddRedeemers [(input_SellOffer_UTxO, SellOfferT.mkDepositRedeemer newDeposit_FT newDeposit_ADA)]
            |> setOutputs [output_SellOffer_UTxO]
            |> setSignatories [tpSellOfferAdmin tp]
            |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------

sellOffer_Withdraw_TxContext :: TestParams -> Integer ->  Integer -> LedgerApiV2.ScriptContext
sellOffer_Withdraw_TxContext tp !newWithdraw_FT !newWithdraw_ADA =
    let
        input_SellOffer_UTxO = sellOffer_UTxO_MockData tp
        -----------------
        input_SellOffer_Datum = SellOfferT.getSellOffer_DatumType_From_UTxO input_SellOffer_UTxO
        input_SellOffer_Value = LedgerApiV2.txOutValue input_SellOffer_UTxO
        -----------------
        output_SellOffer_Datum = SellOffer.mkUpdated_SellOffer_Datum_With_Withdraw
                input_SellOffer_Datum
                newWithdraw_FT newWithdraw_ADA
        -----------------
        newAmount_ADA = SellOfferT.sodAmount_ADA_Available output_SellOffer_Datum + SellOfferT.sodMinADA output_SellOffer_Datum
        newAmount_FT = SellOfferT.sodAmount_FT_Available output_SellOffer_Datum
        -----------------
        adjustedValue_ADA =
            changeValue_Amount
                input_SellOffer_Value
                OnChainHelpers.adaAssetClass
                newAmount_ADA
        adjustedValue_FT_And_ADA =
            changeValue_Amount
                adjustedValue_ADA
                (LedgerValue.assetClass (tpFundPolicy_CS tp) (tpFundFT_TN tp))
                newAmount_FT
        -----------------
        output_SellOffer_UTxO = input_SellOffer_UTxO
            { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $
                    SellOfferT.mkDatum output_SellOffer_Datum
            , LedgerApiV2.txOutValue = adjustedValue_FT_And_ADA
            }
        -----------------
    in
        mkContext
            |> setInputsRef [fund_UTxO_MockData tp]
            |> setInputsAndAddRedeemers [(input_SellOffer_UTxO, SellOfferT.mkWithdrawRedeemer newWithdraw_FT newWithdraw_ADA)]  
            |> setOutputs [output_SellOffer_UTxO]
            |> setSignatories [tpSellOfferAdmin tp]
            |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------

sellOffer_SwapFTxADA_TxContext :: TestParams -> Integer -> Integer -> Integer -> Integer -> Integer -> LedgerApiV2.ScriptContext
sellOffer_SwapFTxADA_TxContext tp !commission_InBPx1e3 !token_FT_Price1xe6 !amount_FT !amount_ADA !commission_ADA =
    let
        --------
        input_SellOffer_UTxO' = sellOffer_UTxO_MockData tp
        -----------------
        input_SellOffer_Datum' = SellOfferT.getSellOffer_DatumType_From_UTxO input_SellOffer_UTxO'
        -----------------
        input_SellOffer_Datum = input_SellOffer_Datum' {
                SellOfferT.sodAskedCommission_InBPx1e3 = commission_InBPx1e3
            }
        -----------------
        input_SellOffer_Value = LedgerApiV2.txOutValue input_SellOffer_UTxO'
        -----------------
        input_SellOffer_UTxO = input_SellOffer_UTxO' {
                LedgerApiV2.txOutDatum =
                    LedgerApiV2.OutputDatum $
                        SellOfferT.mkDatum input_SellOffer_Datum
            }
        -----------------
        output_SellOffer_Datum = SellOffer.mkUpdated_SellOffer_Datum_With_SwapFTxADA
                input_SellOffer_Datum
                amount_FT amount_ADA commission_ADA
        -----------------
        oracleData = mkOracleData tp token_FT_Price1xe6 (tpTransactionDate tp)
        oracleDataSignature = mkOracleDataSignature tp oracleData
        -----------------
        newAmount_ADA = SellOfferT.sodAmount_ADA_Available output_SellOffer_Datum + SellOfferT.sodMinADA output_SellOffer_Datum
        newAmount_FT = SellOfferT.sodAmount_FT_Available output_SellOffer_Datum
        -----------------
        adjustedValue_ADA =
            changeValue_Amount
                input_SellOffer_Value
                OnChainHelpers.adaAssetClass
                newAmount_ADA
        adjustedValue_FT_And_ADA =
            changeValue_Amount
                adjustedValue_ADA
                (LedgerValue.assetClass (tpFundPolicy_CS tp) (tpFundFT_TN tp))
                newAmount_FT
        -----------------
        output_SellOffer_UTxO = input_SellOffer_UTxO
            { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $
                    SellOfferT.mkDatum output_SellOffer_Datum
            , LedgerApiV2.txOutValue = adjustedValue_FT_And_ADA
            }
        -----------------
    in
        mkContext
            |> setInputsRef [protocol_UTxO_MockData tp, fund_UTxO_MockData tp]
            |> setInputsAndAddRedeemers [(input_SellOffer_UTxO, SellOfferT.mkSwapFTxADARedeemer amount_FT amount_ADA commission_ADA oracleData oracleDataSignature)]
            |> setOutputs [output_SellOffer_UTxO]
            |> setSignatories (tpFundAdmins tp)
            |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------

sellOffer_SwapADAxFT_TxContext :: TestParams -> Integer -> Integer -> Integer -> Integer -> Integer -> LedgerApiV2.ScriptContext
sellOffer_SwapADAxFT_TxContext tp !commission_InBPx1e3 !token_FT_Price1xe6 !amount_ADA !amount_FT !commission_FT =
    let
        --------
        input_SellOffer_UTxO' = sellOffer_UTxO_MockData tp
        -----------------
        input_SellOffer_Datum' = SellOfferT.getSellOffer_DatumType_From_UTxO input_SellOffer_UTxO'
        -----------------
        input_SellOffer_Datum = input_SellOffer_Datum' {
                SellOfferT.sodAskedCommission_InBPx1e3 = commission_InBPx1e3
            }
        -----------------
        input_SellOffer_Value = LedgerApiV2.txOutValue input_SellOffer_UTxO'
        -----------------
        input_SellOffer_UTxO = input_SellOffer_UTxO' {
                LedgerApiV2.txOutDatum =
                    LedgerApiV2.OutputDatum $
                        SellOfferT.mkDatum input_SellOffer_Datum
            }
        -----------------
        output_SellOffer_Datum = SellOffer.mkUpdated_SellOffer_Datum_With_SwapADAxFT
                input_SellOffer_Datum
                amount_ADA amount_FT commission_FT
        -----------------
        oracleData = mkOracleData tp token_FT_Price1xe6 (tpTransactionDate tp)
        oracleDataSignature = mkOracleDataSignature tp oracleData
        -----------------
        newAmount_ADA = SellOfferT.sodAmount_ADA_Available output_SellOffer_Datum + SellOfferT.sodMinADA output_SellOffer_Datum
        newAmount_FT = SellOfferT.sodAmount_FT_Available output_SellOffer_Datum
        -----------------
        adjustedValue_ADA =
            changeValue_Amount
                input_SellOffer_Value
                OnChainHelpers.adaAssetClass
                newAmount_ADA
        adjustedValue_FT_And_ADA =
            changeValue_Amount
                adjustedValue_ADA
                (LedgerValue.assetClass (tpFundPolicy_CS tp) (tpFundFT_TN tp))
                newAmount_FT
        -----------------
        output_SellOffer_UTxO = input_SellOffer_UTxO
            { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $
                    SellOfferT.mkDatum output_SellOffer_Datum
            , LedgerApiV2.txOutValue = adjustedValue_FT_And_ADA
            }
        -----------------
    in
        mkContext
            |> setInputsRef [protocol_UTxO_MockData tp, fund_UTxO_MockData tp]
            |> setInputsAndAddRedeemers [(input_SellOffer_UTxO, SellOfferT.mkSwapADAxFTRedeemer amount_ADA amount_FT commission_FT oracleData oracleDataSignature)]
            |> setOutputs [output_SellOffer_UTxO]
            |> setSignatories (tpFundAdmins tp)
            |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------
