--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

{- |
Module      : TestUtils.Contracts.TxSpecs.SellOffer
Description : Mock Data and Auxiliary Functions for testing the SellOffer.
-}
module TestUtils.Contracts.TxSpecs.SellOffer where

--------------------------------------------------------------------------------3

-- Non-IOG imports

import qualified Data.List                            as DataList
import qualified Prelude                              as P

-- IOG imports
import qualified Ledger
import qualified Ledger.Value                         as LedgerValue
import qualified Plutus.V2.Ledger.Api                 as LedgerApiV2
import           PlutusTx.Prelude

-- Project imports

import qualified Generic.OnChainHelpers               as OnChainHelpers
import qualified Protocol.Constants                   as T
import qualified Protocol.Protocol.Types              as ProtocolT
import qualified Protocol.SellOffer.OnChain           as SellOffer
import qualified Protocol.SellOffer.Types             as SellOfferT
import qualified Protocol.SellOffer.Types             as T
import qualified Protocol.Types                       as T
import           TestUtils.Automatic.ContextGenerator
import           TestUtils.Automatic.Helpers
import           TestUtils.Automatic.HelpersMAYZ
import           TestUtils.Automatic.Types
import           TestUtils.Constants
import           TestUtils.Contracts.InitialData
import           TestUtils.Helpers
import           TestUtils.TestContext.Helpers
import           TestUtils.Types
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------
-- SellOffer Contract
--------------------------------------------------------------------------------

sellOffer_Create_TxSpecs :: TestParams -> TxSpecs
sellOffer_Create_TxSpecs tp =
    let
        -----------------
        input_Protocol_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                (protocol_UTxO_MockData tp)
                Protocol_TestEntity
                op
        input_Fund_UTxO_gen op _ =
            txOut_With_TestEntity_Gen tp (fund_UTxO_MockData tp) Fund_TestEntity op
        -----------------
        output_SellOffer_UTxO_gen op extras =
            let
                tempTxOut =
                    txOut_With_TestEntity_Gen
                        tp
                        (sellOffer_UTxO_MockData tp)
                        SellOffer_TestEntity
                        op
            in
                -- aca voy a seguir manipulando la lista de txOuts generadas en funcion de los extras
                case DataList.find P.snd extras of
                    -- cuando se usan extras, todos los test params son en valido
                    -- por lo tanto se que hay una sola y valida txOut
                    Just ("total_FT_Earned different from 0", _) ->
                        let
                            datum = SellOfferT.getSellOffer_DatumType_From_UTxO (head tempTxOut)
                            datumUpdated =
                                datum {SellOfferT.sodTotal_FT_Earned = 1}
                        in
                            [ (head tempTxOut)
                                { LedgerApiV2.txOutDatum =
                                    LedgerApiV2.OutputDatum $
                                        SellOfferT.mkDatum datumUpdated
                                }
                            ]
                    _ -> tempTxOut

        -----------------
        mint_SellOfferID_gen op _ =
            mint_Value_With_TestToken_Gen
                tp
                (SellOfferID_TestToken, SellOffer_MintID_TestRedeemer)
                1
                op
        -----------------
        validityRange_gen' op _ = validityRange_gen tp (tpTransactionDate tp) op
    in
        -----------------
        TxSpecs
            { tsInputsRef =
                [ (Protocol_TestEntity, input_Protocol_UTxO_gen)
                , (Fund_TestEntity, input_Fund_UTxO_gen)
                ]
            , tsInputsRefScripts = []
            , tsInputs = []
            , tsInputsFromWallet = []
            , tsOutputs = [(SellOffer_TestEntity, output_SellOffer_UTxO_gen)]
            , tsMints =
                [
                    ( SellOfferID_TestToken
                    , mint_SellOfferID_gen
                    , SellOffer_MintID_TestRedeemer
                    )
                ]
            , tsUseSignatures = Nothing
            , tsUseValidityRange = Just validityRange_gen'
            , tsExtras = [("total_FT_Earned different from 0", False)]
            }

--------------------------------------------------------------------------------

sellOffer_Delete_TxSpecs :: TestParams -> TxSpecs
sellOffer_Delete_TxSpecs tp =
    -----------------
    let
        input_SellOffer_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                (sellOffer_UTxO_MockData tp)
                SellOffer_TestEntity
                op
        -----------------
        consume_SellOffer_ValidRedeemerData = SellOfferT.mkDeleteRedeemer
        consume_SellOffer_InvalidRedeemerData = Nothing
        consume_SellOffer_InvalidRedeemerType =
            Just SellOfferT.mkUpdateMinADARedeemer
        consume_SellOffer_InvalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        consume_SellOffer_UTxO_gen =
            consume_TxOut_Gen
                input_SellOffer_UTxO_gen
                consume_SellOffer_ValidRedeemerData
                consume_SellOffer_InvalidRedeemerData
                consume_SellOffer_InvalidRedeemerType
                consume_SellOffer_InvalidRedeemerNonExist
        -----------------
        burn_SellOfferID_gen op _ =
            mint_Value_With_TestToken_Gen
                tp
                (SellOfferID_TestToken, SellOffer_BurnID_TestRedeemer)
                1
                op
        -----------------
        signatures_gen' op _ = signatures_gen tp [tpSellOfferAdmin tp] op
        -----------------
        validityRange_gen' op _ = validityRange_gen tp (tpTransactionDate tp) op
    in
        -----------------
        TxSpecs
            { tsInputsRef = []
            , tsInputsRefScripts = []
            , tsInputs =
                [
                    ( SellOffer_TestEntity
                    , consume_SellOffer_UTxO_gen
                    , SellOffer_Delete_TestRedeemer
                    )
                ]
            , tsInputsFromWallet = []
            , tsOutputs = []
            , tsMints =
                [
                    ( SellOfferID_TestToken
                    , burn_SellOfferID_gen
                    , SellOffer_BurnID_TestRedeemer
                    )
                ]
            , tsUseSignatures = Just signatures_gen'
            , tsUseValidityRange = Just validityRange_gen'
            , tsExtras = []
            }

--------------------------------------------------------------------------------

sellOffer_UpdateStatus_TxSpecs :: TestParams -> TxSpecs
sellOffer_UpdateStatus_TxSpecs tp =
    -----------------
    let
        input_Fund_UTxO_gen op _ =
            txOut_With_TestEntity_Gen tp (fund_UTxO_MockData tp) Fund_TestEntity op
        -----------------
        input_SellOffer_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                (sellOffer_UTxO_MockData tp)
                SellOffer_TestEntity
                op
        -----------------
        input_SellOffer_Datum = SellOfferT.getSellOffer_DatumType_From_UTxO (sellOffer_UTxO_MockData tp)
        -----------------
        newStatus = T.sellOffer_Status_Closed
        -----------------
        output_SellOffer_Datum =
            SellOffer.mkUpdated_SellOffer_Datum_With_StatusChanged
                input_SellOffer_Datum
                newStatus
        -----------------
        output_SellOffer_UTxO =
            (sellOffer_UTxO_MockData tp)
                { LedgerApiV2.txOutDatum =
                    LedgerApiV2.OutputDatum $
                        SellOfferT.mkDatum output_SellOffer_Datum
                }
        -----------------
        output_SellOffer_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                output_SellOffer_UTxO
                SellOffer_TestEntity
                op
        -----------------
        consume_SellOffer_ValidRedeemerData =
            SellOfferT.mkUpdateStatusRedeemer newStatus
        consume_SellOffer_InvalidRedeemerData =
            Just (SellOfferT.mkUpdateStatusRedeemer T.sellOffer_Status_Open)
        consume_SellOffer_InvalidRedeemerType =
            Just SellOfferT.mkUpdateMinADARedeemer
        consume_SellOffer_InvalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        consume_SellOffer_UTxO_gen =
            consume_TxOut_Gen
                input_SellOffer_UTxO_gen
                consume_SellOffer_ValidRedeemerData
                consume_SellOffer_InvalidRedeemerData
                consume_SellOffer_InvalidRedeemerType
                consume_SellOffer_InvalidRedeemerNonExist
        -----------------
        signatures_gen' op _ = signatures_gen tp [tpSellOfferAdmin tp] op
        -----------------
        validityRange_gen' op _ = validityRange_gen tp (tpTransactionDate tp) op
    in
        -----------------
        TxSpecs
            { tsInputsRef = [(Fund_TestEntity, input_Fund_UTxO_gen)]
            , tsInputsRefScripts = []
            , tsInputs =
                [
                    ( SellOffer_TestEntity
                    , consume_SellOffer_UTxO_gen
                    , SellOffer_UpdateStatus_TestRedeemer
                    )
                ]
            , tsInputsFromWallet = []
            , tsOutputs = [(SellOffer_TestEntity, output_SellOffer_UTxO_gen)]
            , tsMints = []
            , tsUseSignatures = Just signatures_gen'
            , tsUseValidityRange = Just validityRange_gen'
            , tsExtras = []
            }

--------------------------------------------------------------------------------

sellOffer_UpdateAskedCommissionRate_TxSpecs :: TestParams -> TxSpecs
sellOffer_UpdateAskedCommissionRate_TxSpecs tp =
    -----------------
    let
        input_Protocol_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                (protocol_UTxO_MockData tp)
                Protocol_TestEntity
                op
        input_Fund_UTxO_gen op _ =
            txOut_With_TestEntity_Gen tp (fund_UTxO_MockData tp) Fund_TestEntity op
        -----------------
        input_SellOffer_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                (sellOffer_UTxO_MockData tp)
                SellOffer_TestEntity
                op
        -----------------
        input_SellOffer_Datum = SellOfferT.getSellOffer_DatumType_From_UTxO (sellOffer_UTxO_MockData tp)
        -----------------
        askedCommissions =
            ProtocolT.mmdMax $
                ProtocolT.pdCommissionSellOffer_InBPx1e3 $
                    protocol_DatumType_MockData tp
        -----------------
        output_SellOffer_Datum =
            SellOffer.mkUpdated_SellOffer_Datum_With_CommissionChanged
                input_SellOffer_Datum
                askedCommissions
        -----------------
        output_SellOffer_UTxO =
            (sellOffer_UTxO_MockData tp)
                { LedgerApiV2.txOutDatum =
                    LedgerApiV2.OutputDatum $
                        SellOfferT.mkDatum output_SellOffer_Datum
                }
        -----------------
        output_SellOffer_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                output_SellOffer_UTxO
                SellOffer_TestEntity
                op
        -----------------
        consume_SellOffer_ValidRedeemerData =
            SellOfferT.mkUpdateAskedCommissionRateRedeemer askedCommissions
        consume_SellOffer_InvalidRedeemerData =
            Just
                (SellOfferT.mkUpdateAskedCommissionRateRedeemer (askedCommissions + 1))
        consume_SellOffer_InvalidRedeemerType =
            Just SellOfferT.mkUpdateMinADARedeemer
        consume_SellOffer_InvalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        consume_SellOffer_UTxO_gen =
            consume_TxOut_Gen
                input_SellOffer_UTxO_gen
                consume_SellOffer_ValidRedeemerData
                consume_SellOffer_InvalidRedeemerData
                consume_SellOffer_InvalidRedeemerType
                consume_SellOffer_InvalidRedeemerNonExist
        -----------------
        signatures_gen' op _ = signatures_gen tp [tpSellOfferAdmin tp] op
        -----------------
        validityRange_gen' op _ = validityRange_gen tp (tpTransactionDate tp) op
    in
        -----------------
        TxSpecs
            { tsInputsRef =
                [ (Protocol_TestEntity, input_Protocol_UTxO_gen)
                , (Fund_TestEntity, input_Fund_UTxO_gen)
                ]
            , tsInputsRefScripts = []
            , tsInputs =
                [
                    ( SellOffer_TestEntity
                    , consume_SellOffer_UTxO_gen
                    , SellOffer_UpdateStatus_TestRedeemer
                    )
                ]
            , tsInputsFromWallet = []
            , tsOutputs = [(SellOffer_TestEntity, output_SellOffer_UTxO_gen)]
            , tsMints = []
            , tsUseSignatures = Just signatures_gen'
            , tsUseValidityRange = Just validityRange_gen'
            , tsExtras = []
            }

--------------------------------------------------------------------------------

sellOffer_UpdateSellRestrictions_TxSpecs :: TestParams -> TxSpecs
sellOffer_UpdateSellRestrictions_TxSpecs tp =
    -----------------
    let
        input_Fund_UTxO_gen op _ =
            txOut_With_TestEntity_Gen tp (fund_UTxO_MockData tp) Fund_TestEntity op
        -----------------
        input_SellOffer_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                (sellOffer_UTxO_MockData tp)
                SellOffer_TestEntity
                op
        -----------------
        input_SellOffer_Datum = SellOfferT.getSellOffer_DatumType_From_UTxO (sellOffer_UTxO_MockData tp)
        -----------------
        sellFT = T.sellOffer_NotAllowSell
        sellADA = T.sellOffer_NotAllowSell
        -----------------
        output_SellOffer_Datum =
            SellOffer.mkUpdated_SellOffer_Datum_With_RestrictionsChanged
                input_SellOffer_Datum
                sellFT
                sellADA
        -----------------
        output_SellOffer_UTxO =
            (sellOffer_UTxO_MockData tp)
                { LedgerApiV2.txOutDatum =
                    LedgerApiV2.OutputDatum $
                        SellOfferT.mkDatum output_SellOffer_Datum
                }
        -----------------
        output_SellOffer_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                output_SellOffer_UTxO
                SellOffer_TestEntity
                op
        -----------------
        consume_SellOffer_ValidRedeemerData =
            SellOfferT.mkUpdateSellRestrictionsRedeemer sellFT sellADA
        consume_SellOffer_InvalidRedeemerData =
            Just
                ( SellOfferT.mkUpdateSellRestrictionsRedeemer
                    T.sellOffer_AllowSell
                    T.sellOffer_NotAllowSell
                )
        consume_SellOffer_InvalidRedeemerType =
            Just SellOfferT.mkUpdateMinADARedeemer
        consume_SellOffer_InvalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        consume_SellOffer_UTxO_gen =
            consume_TxOut_Gen
                input_SellOffer_UTxO_gen
                consume_SellOffer_ValidRedeemerData
                consume_SellOffer_InvalidRedeemerData
                consume_SellOffer_InvalidRedeemerType
                consume_SellOffer_InvalidRedeemerNonExist
        -----------------
        signatures_gen' op _ = signatures_gen tp [tpSellOfferAdmin tp] op
        -----------------
        validityRange_gen' op _ = validityRange_gen tp (tpTransactionDate tp) op
    in
        -----------------
        TxSpecs
            { tsInputsRef = [(Fund_TestEntity, input_Fund_UTxO_gen)]
            , tsInputsRefScripts = []
            , tsInputs =
                [
                    ( SellOffer_TestEntity
                    , consume_SellOffer_UTxO_gen
                    , SellOffer_UpdateStatus_TestRedeemer
                    )
                ]
            , tsInputsFromWallet = []
            , tsOutputs = [(SellOffer_TestEntity, output_SellOffer_UTxO_gen)]
            , tsMints = []
            , tsUseSignatures = Just signatures_gen'
            , tsUseValidityRange = Just validityRange_gen'
            , tsExtras = []
            }

--------------------------------------------------------------------------------

sellOffer_UpdateMinADA_TxSpecs :: TestParams -> [TxParam] -> TxSpecs
sellOffer_UpdateMinADA_TxSpecs tp txParams =
    let
        -----------------
        -- min ADA es el que se va a establecer en salida
        -- available ADA es el que se va a establecer en la entrada
        newMinADA = getTxParam "newMinADA" txParams :: Integer
        availableADA = getTxParam "availableADA" txParams :: Integer
        -----------------
        input_Fund_UTxO_gen op _ =
            txOut_With_TestEntity_Gen tp (fund_UTxO_MockData tp) Fund_TestEntity op
        -----------------
        -- de la SellOffer original, quiero editar el ada available
        -----------------
        input_SellOffer_Datum' = SellOfferT.getSellOffer_DatumType_From_UTxO (sellOffer_UTxO_MockData tp)
        -----------------
        input_SellOffer_Datum =
            input_SellOffer_Datum'
                { SellOfferT.sodAmount_ADA_Available = availableADA
                }
        -----------------
        input_SellOffer_UTxO =
            let
                origValue' = LedgerApiV2.txOutValue $ sellOffer_UTxO_MockData tp
                newAmount' =
                    SellOfferT.sodAmount_ADA_Available input_SellOffer_Datum
                        + SellOfferT.sodMinADA input_SellOffer_Datum
            in
                (sellOffer_UTxO_MockData tp)
                    { LedgerApiV2.txOutDatum =
                        LedgerApiV2.OutputDatum $
                            SellOfferT.mkDatum input_SellOffer_Datum
                    , LedgerApiV2.txOutValue =
                        changeValue_Amount
                            origValue'
                            OnChainHelpers.adaAssetClass
                            newAmount'
                    }
        -----------------
        input_SellOffer_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                input_SellOffer_UTxO
                SellOffer_TestEntity
                op
        -----------------
        output_SellOffer_Datum =
            SellOffer.mkUpdated_SellOffer_Datum_With_MinADAChanged
                input_SellOffer_Datum
                newMinADA
        -----------------
        origValue = LedgerApiV2.txOutValue input_SellOffer_UTxO
        newAmount =
            SellOfferT.sodAmount_ADA_Available output_SellOffer_Datum
                + SellOfferT.sodMinADA output_SellOffer_Datum
        -----------------
        output_SellOffer_UTxO =
            input_SellOffer_UTxO
                { LedgerApiV2.txOutDatum =
                    LedgerApiV2.OutputDatum $
                        SellOfferT.mkDatum output_SellOffer_Datum
                , LedgerApiV2.txOutValue =
                    changeValue_Amount
                        origValue
                        OnChainHelpers.adaAssetClass
                        newAmount
                }
        -----------------
        output_SellOffer_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                output_SellOffer_UTxO
                SellOffer_TestEntity
                op
        -----------------
        consume_SellOffer_ValidRedeemerData = SellOfferT.mkUpdateMinADARedeemer
        consume_SellOffer_InvalidRedeemerData = Nothing
        consume_SellOffer_InvalidRedeemerType = Just SellOfferT.mkDeleteRedeemer
        consume_SellOffer_InvalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        consume_SellOffer_UTxO_gen =
            consume_TxOut_Gen
                input_SellOffer_UTxO_gen
                consume_SellOffer_ValidRedeemerData
                consume_SellOffer_InvalidRedeemerData
                consume_SellOffer_InvalidRedeemerType
                consume_SellOffer_InvalidRedeemerNonExist
        -----------------
        signatures_gen' op _ = signatures_gen tp [tpSellOfferAdmin tp] op
        -----------------
        validityRange_gen' op _ = validityRange_gen tp (tpTransactionDate tp) op
    in
        -----------------
        TxSpecs
            { tsInputsRef = [(Fund_TestEntity, input_Fund_UTxO_gen)]
            , tsInputsRefScripts = []
            , tsInputs =
                [
                    ( SellOffer_TestEntity
                    , consume_SellOffer_UTxO_gen
                    , SellOffer_UpdateMinADA_TestRedeemer
                    )
                ]
            , tsInputsFromWallet = []
            , tsOutputs = [(SellOffer_TestEntity, output_SellOffer_UTxO_gen)]
            , tsMints = []
            , tsUseSignatures = Just signatures_gen'
            , tsUseValidityRange = Just validityRange_gen'
            , tsExtras = [("Valid Min ADA positive", True), ("Invalid Min ADA negative", True)]
            }

--------------------------------------------------------------------------------

sellOffer_Deposit_TxSpecs :: TestParams -> Integer -> Integer -> TxSpecs
sellOffer_Deposit_TxSpecs tp !newDeposit_FT !newDeposit_ADA =
    -----------------
    let
        input_Fund_UTxO_gen op _ =
            txOut_With_TestEntity_Gen tp (fund_UTxO_MockData tp) Fund_TestEntity op
        -----------------
        input_SellOffer_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                (sellOffer_UTxO_MockData tp)
                SellOffer_TestEntity
                op
        -----------------
        input_SellOffer_Datum = SellOfferT.getSellOffer_DatumType_From_UTxO (sellOffer_UTxO_MockData tp)
        -----------------
        output_SellOffer_Datum =
            SellOffer.mkUpdated_SellOffer_Datum_With_Deposit
                input_SellOffer_Datum
                newDeposit_FT
                newDeposit_ADA
        -----------------
        output_SellOffer_UTxO =
            let
                newDatum = output_SellOffer_Datum
                origValue = LedgerApiV2.txOutValue $ sellOffer_UTxO_MockData tp
                newAmount_ADA =
                    SellOfferT.sodAmount_ADA_Available newDatum
                        + SellOfferT.sodMinADA newDatum
                newAmount_FT = SellOfferT.sodAmount_FT_Available newDatum
                -----------------
                adjustedValue_ADA =
                    changeValue_Amount
                        origValue
                        OnChainHelpers.adaAssetClass
                        newAmount_ADA
                adjustedValue_FT_And_ADA =
                    changeValue_Amount
                        adjustedValue_ADA
                        (LedgerValue.assetClass (tpFundPolicy_CS tp) (tpFundFT_TN tp))
                        newAmount_FT
            in
                (sellOffer_UTxO_MockData tp)
                    { LedgerApiV2.txOutDatum =
                        LedgerApiV2.OutputDatum $ SellOfferT.mkDatum newDatum
                    , LedgerApiV2.txOutValue = adjustedValue_FT_And_ADA
                    }
        -----------------
        output_SellOffer_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                output_SellOffer_UTxO
                SellOffer_TestEntity
                op
        -----------------
        consume_SellOffer_ValidRedeemerData =
            SellOfferT.mkDepositRedeemer newDeposit_FT newDeposit_ADA
        consume_SellOffer_InvalidRedeemerData =
            Just (SellOfferT.mkDepositRedeemer (newDeposit_FT + 1) newDeposit_ADA)
        consume_SellOffer_InvalidRedeemerType =
            Just SellOfferT.mkUpdateMinADARedeemer
        consume_SellOffer_InvalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        consume_SellOffer_UTxO_gen =
            consume_TxOut_Gen
                input_SellOffer_UTxO_gen
                consume_SellOffer_ValidRedeemerData
                consume_SellOffer_InvalidRedeemerData
                consume_SellOffer_InvalidRedeemerType
                consume_SellOffer_InvalidRedeemerNonExist
        -----------------
        signatures_gen' op _ = signatures_gen tp [tpSellOfferAdmin tp] op
        -----------------
        validityRange_gen' op _ = validityRange_gen tp (tpTransactionDate tp) op
    in
        -----------------
        TxSpecs
            { tsInputsRef = [(Fund_TestEntity, input_Fund_UTxO_gen)]
            , tsInputsRefScripts = []
            , tsInputs =
                [
                    ( SellOffer_TestEntity
                    , consume_SellOffer_UTxO_gen
                    , SellOffer_Deposit_TestRedeemer
                    )
                ]
            , tsInputsFromWallet = []
            , tsOutputs = [(SellOffer_TestEntity, output_SellOffer_UTxO_gen)]
            , tsMints = []
            , tsUseSignatures = Just signatures_gen'
            , tsUseValidityRange = Just validityRange_gen'
            , tsExtras = []
            }

--------------------------------------------------------------------------------

sellOffer_Withdraw_TxSpecs :: TestParams -> Integer -> Integer -> TxSpecs
sellOffer_Withdraw_TxSpecs tp !newWithdraw_FT !newWithdraw_ADA =
    -----------------
    let
        input_Fund_UTxO_gen op _ =
            txOut_With_TestEntity_Gen tp (fund_UTxO_MockData tp) Fund_TestEntity op
        -----------------
        input_SellOffer_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                (sellOffer_UTxO_MockData tp)
                SellOffer_TestEntity
                op
        -----------------
        input_SellOffer_Datum = SellOfferT.getSellOffer_DatumType_From_UTxO (sellOffer_UTxO_MockData tp)
        -----------------
        output_SellOffer_Datum =
            SellOffer.mkUpdated_SellOffer_Datum_With_Withdraw
                input_SellOffer_Datum
                newWithdraw_FT
                newWithdraw_ADA
        -----------------
        output_SellOffer_UTxO =
            let
                newDatum = output_SellOffer_Datum
                origValue = LedgerApiV2.txOutValue $ sellOffer_UTxO_MockData tp
                newAmount_ADA =
                    SellOfferT.sodAmount_ADA_Available newDatum
                        + SellOfferT.sodMinADA newDatum
                newAmount_FT = SellOfferT.sodAmount_FT_Available newDatum
                -----------------
                adjustedValue_ADA =
                    changeValue_Amount
                        origValue
                        OnChainHelpers.adaAssetClass
                        newAmount_ADA
                adjustedValue_FT_And_ADA =
                    changeValue_Amount
                        adjustedValue_ADA
                        (LedgerValue.assetClass (tpFundPolicy_CS tp) (tpFundFT_TN tp))
                        newAmount_FT
            in
                (sellOffer_UTxO_MockData tp)
                    { LedgerApiV2.txOutDatum =
                        LedgerApiV2.OutputDatum $ SellOfferT.mkDatum newDatum
                    , LedgerApiV2.txOutValue = adjustedValue_FT_And_ADA
                    }
        -----------------
        output_SellOffer_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                output_SellOffer_UTxO
                SellOffer_TestEntity
                op
        -----------------
        consume_SellOffer_ValidRedeemerData =
            SellOfferT.mkWithdrawRedeemer newWithdraw_FT newWithdraw_ADA
        consume_SellOffer_InvalidRedeemerData =
            Just
                (SellOfferT.mkWithdrawRedeemer (newWithdraw_FT + 1) newWithdraw_ADA)
        consume_SellOffer_InvalidRedeemerType =
            Just SellOfferT.mkUpdateMinADARedeemer
        consume_SellOffer_InvalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        consume_SellOffer_UTxO_gen =
            consume_TxOut_Gen
                input_SellOffer_UTxO_gen
                consume_SellOffer_ValidRedeemerData
                consume_SellOffer_InvalidRedeemerData
                consume_SellOffer_InvalidRedeemerType
                consume_SellOffer_InvalidRedeemerNonExist
        -----------------
        signatures_gen' op _ = signatures_gen tp [tpSellOfferAdmin tp] op
        -----------------
        validityRange_gen' op _ = validityRange_gen tp (tpTransactionDate tp) op
    in
        -----------------
        TxSpecs
            { tsInputsRef = [(Fund_TestEntity, input_Fund_UTxO_gen)]
            , tsInputsRefScripts = []
            , tsInputs =
                [
                    ( SellOffer_TestEntity
                    , consume_SellOffer_UTxO_gen
                    , SellOffer_Deposit_TestRedeemer
                    )
                ]
            , tsInputsFromWallet = []
            , tsOutputs = [(SellOffer_TestEntity, output_SellOffer_UTxO_gen)]
            , tsMints = []
            , tsUseSignatures = Just signatures_gen'
            , tsUseValidityRange = Just validityRange_gen'
            , tsExtras = []
            }

--------------------------------------------------------------------------------

sellOffer_SwapFTxADA_TxSpecs :: TestParams -> [TxParam] -> TxSpecs
sellOffer_SwapFTxADA_TxSpecs tp txParams =
    let
        -----------------
        token_FT_Price1xe6 = getTxParam "token_FT_Price1xe6" txParams :: Integer
        amount_FT = getTxParam "amount_FT" txParams :: Integer
        available_ADA = getTxParam "available_ADA" txParams :: Integer
        -----------------
        oracleData = mkOracleData tp token_FT_Price1xe6 (tpTransactionDate tp)
        -----------------
        input_Protocol_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                (protocol_UTxO_MockData tp)
                Protocol_TestEntity
                op
        -----------------
        input_Fund_UTxO_gen op _ =
            txOut_With_TestEntity_Gen tp (fund_UTxO_MockData tp) Fund_TestEntity op
        -----------------
        -- Este es el generador. Puede generar zero, una o muchas txOuts, validas o invalidas.
        input_SellOffer_UTxO_gen op extras =
            -- aca el generador de txOut me genera una lista de txOuts segun test params
            let
                input_SellOffer_UTxO = sellOffer_UTxO_MockData_Parametrizable tp 5_000_000 available_ADA
                txOuts =
                    txOut_With_TestEntity_Gen
                        tp
                        input_SellOffer_UTxO
                        SellOffer_TestEntity
                        op
            in
                -- aca voy a seguir manipulando la lista de txOuts generadas en funcion de los extras
                case DataList.find P.snd extras of
                    -- cuando se usan extras, todos los test params son en valido
                    -- por lo tanto se que hay una sola y valida txOut
                    Just ("not isOrderOpen", _) ->
                        let
                            datum = SellOfferT.getSellOffer_DatumType_From_UTxO (head txOuts)
                            datumUpdated =
                                SellOffer.mkUpdated_SellOffer_Datum_With_StatusChanged
                                    datum
                                    T.sellOffer_Status_Closed
                        in
                            [ (head txOuts)
                                { LedgerApiV2.txOutDatum =
                                    LedgerApiV2.OutputDatum $
                                        SellOfferT.mkDatum datumUpdated
                                }
                            ]
                    Just ("isOrderRestrictedForSellingADA", True) ->
                        let
                            datum = SellOfferT.getSellOffer_DatumType_From_UTxO (head txOuts)
                            datumUpdated =
                                SellOffer.mkUpdated_SellOffer_Datum_With_RestrictionsChanged
                                    datum
                                    T.sellOffer_AllowSell
                                    T.sellOffer_NotAllowSell
                        in
                            [ (head txOuts)
                                { LedgerApiV2.txOutDatum =
                                    LedgerApiV2.OutputDatum $
                                        SellOfferT.mkDatum datumUpdated
                                }
                            ]
                    Just ("isOrderRestrictedForSellingFT", True) ->
                        let
                            datum = SellOfferT.getSellOffer_DatumType_From_UTxO (head txOuts)
                            datumUpdated =
                                SellOffer.mkUpdated_SellOffer_Datum_With_RestrictionsChanged
                                    datum
                                    T.sellOffer_NotAllowSell
                                    T.sellOffer_AllowSell
                        in
                            [ (head txOuts)
                                { LedgerApiV2.txOutDatum =
                                    LedgerApiV2.OutputDatum $
                                        SellOfferT.mkDatum datumUpdated
                                }
                            ]
                    Just ("not isAmount_ADA_Available", True) ->
                        -- Uso txOuts como vienen, por que los valores se generan con los parametros del property test
                        txOuts
                    _ ->
                        txOuts
        -----------------
        -- Este se usa para calcular los outputs txOuts. Se necesita al menos una entrada para hacer eso
        input_SellOffer_UTxO_gen_for_calcs op extras =
            case op of
                TxOutInvalid (TxOutInvalidEntity (TxOutInvalidEntityDatum InvalidEntityDatumType)) -> sellOffer_UTxO_MockData tp
                TxOutInvalid (TxOutInvalidEntity (TxOutInvalidEntityDatum InvalidEntityDatumNonExist)) -> sellOffer_UTxO_MockData tp
                TxOutInvalid TxOutInvalidNone -> sellOffer_UTxO_MockData tp
                _ -> case input_SellOffer_UTxO_gen op extras of
                    []      -> sellOffer_UTxO_MockData tp
                    (x : _) -> x
        -----------------
        -- Este se usa para calculos internos. Se necesita al menos un datum de entrada para poder hacerlos
        input_SellOffer_Datum' op extras = SellOfferT.getSellOffer_DatumType_From_UTxO (input_SellOffer_UTxO_gen_for_calcs op extras)
        -----------------
        calculateConversion op extras =
            -----------------
            let
                input_SellOffer_Datum = input_SellOffer_Datum' op extras
                -----------------
                (_, _, price_FT_in_ADA) =
                    head $ T.iuValues $ T.odFTPriceADA1xe6 oracleData
                --------
                amount_ADA = OnChainHelpers.multiply_By_Scaled_1e6_And_RoundUp amount_FT price_FT_in_ADA
                --------
                commission = T.sodAskedCommission_InBPx1e3 input_SellOffer_Datum
                commission_ADA = OnChainHelpers.multiply_By_Scaled_BPx1e3_And_RoundUp amount_ADA commission
            in
                --------

                -- modifico los valores de ADA y comision segun los extras
                case DataList.find P.snd extras of
                    Just ("Invalid_Conversion", _) ->
                        let
                            invalidAmount = getTxParam "invalidAmount" txParams :: Integer
                        in
                            (amount_ADA + invalidAmount, commission_ADA)
                    Just ("Invalid_Commissions", _) ->
                        let
                            invalidAmount = getTxParam "invalidAmount" txParams :: Integer
                        in
                            (amount_ADA, commission_ADA + invalidAmount)
                    _ -> (amount_ADA, commission_ADA)
        -----------------
        output_SellOffer_Datum op extras =
            -----------------
            let
                input_SellOffer_Datum = input_SellOffer_Datum' op extras
                -----------------
                (amount_ADA, commission_ADA) = calculateConversion op extras
            in
                -----------------
                SellOffer.mkUpdated_SellOffer_Datum_With_SwapFTxADA
                    input_SellOffer_Datum
                    amount_FT
                    amount_ADA
                    commission_ADA
        -----------------
        output_SellOffer_UTxO op extras =
            let
                newDatum = output_SellOffer_Datum op extras
                -----------------
                origValue =
                    LedgerApiV2.txOutValue $
                        input_SellOffer_UTxO_gen_for_calcs op extras
                newAmount_ADA =
                    SellOfferT.sodAmount_ADA_Available newDatum
                        + SellOfferT.sodMinADA newDatum
                newAmount_FT = SellOfferT.sodAmount_FT_Available newDatum
                -----------------
                adjustedValue_ADA =
                    changeValue_Amount
                        origValue
                        OnChainHelpers.adaAssetClass
                        newAmount_ADA
                -----------------
                adjustedValue_FT_And_ADA =
                    changeValue_Amount
                        adjustedValue_ADA
                        (LedgerValue.assetClass (tpFundPolicy_CS tp) (tpFundFT_TN tp))
                        newAmount_FT
            in
                -----------------
                (input_SellOffer_UTxO_gen_for_calcs op extras)
                    { LedgerApiV2.txOutDatum =
                        LedgerApiV2.OutputDatum $ SellOfferT.mkDatum newDatum
                    , LedgerApiV2.txOutValue = adjustedValue_FT_And_ADA
                    }
        -----------------
        output_SellOffer_UTxO_gen op extras =
            txOut_With_TestEntity_Gen
                tp
                (output_SellOffer_UTxO op extras)
                SellOffer_TestEntity
                op
        -----------------
        consume_SellOffer_ValidRedeemerData op extras =
            let
                (amount_ADA, commission_ADA) =
                    calculateConversion (convertInputOptionsToTxOutOptions op) extras
                -- modifico oracleTokenFTData: el valor de la fecha del oraculo y otras cosas segun los extras
                oracleData' = case DataList.find P.snd extras of
                    Just ("Oracle Time Too Early", _) ->
                        let
                            -- Create the valid range based on the transaction date
                            validRange = createValidRange (tpTransactionDate tp)
                            -- Extract the lower limit from the valid range
                            lowerLimit = case Ledger.ivFrom validRange of
                                Ledger.LowerBound (Ledger.Finite a) True -> a
                                _                                        -> traceError "Invalid interval lower bound"
                            -- Adjust the lower limit by subtracting the valid time
                            newLowerLimitValue = lowerLimit - T.oracleData_Valid_Time
                            -- Set the oracle time to be just before the new lower limit
                            oracleTimeTooEarly = newLowerLimitValue - sum_ONE_INVALID_DATE
                        in
                            -- Create the oracle data with the adjusted time
                            mkOracleData tp token_FT_Price1xe6 oracleTimeTooEarly
                    Just ("Oracle Time Too Late", _) ->
                        let
                            -- Create the valid range based on the transaction date
                            validRange = createValidRange (tpTransactionDate tp)
                            -- Extract the upper limit from the valid range
                            upperLimit = case Ledger.ivTo validRange of
                                Ledger.UpperBound (Ledger.Finite a) _ -> a
                                _                                     -> traceError "Invalid interval upper bound"
                            -- Set the oracle time to be just after the upper limit
                            oracleTimeTooLate = upperLimit + sum_ONE_INVALID_DATE
                        in
                            -- Create the oracle data with the adjusted time
                            mkOracleData tp token_FT_Price1xe6 oracleTimeTooLate
                    Just ("FT Price ADA not found", _) ->
                        let
                            tokenFTPrice1xe6' =
                                T.InvestUnit
                                    -- cambio la poliza de FundID por cualquier otra, la de holding
                                    [ (tpFundHoldingPolicyID_CS tp, tpFundFT_TN tp, token_FT_Price1xe6)
                                    ]
                        in
                            T.Oracle_Data tokenFTPrice1xe6' (tpTransactionDate tp)
                    _ -> oracleData
                -- modifico oracleDataSignature segun los extras
                oracleDataSignature' =
                    case DataList.find P.snd extras of
                        Just ("not isCorrect_Oracle_Signature", _) ->
                            let
                                oracleTokenFTData_Changed = mkOracleData tp token_FT_Price1xe6 (tpTransactionDate tp + sum_ONE_INVALID_DATE)
                            in
                                mkOracleDataSignature tp oracleTokenFTData_Changed
                        _ -> mkOracleDataSignature tp oracleData'
            in
                SellOfferT.mkSwapFTxADARedeemer
                    amount_FT
                    amount_ADA
                    commission_ADA
                    oracleData'
                    oracleDataSignature'
        -----------------
        consume_SellOffer_InvalidRedeemerData op extras =
            let
                (amount_ADA, commission_ADA) =
                    calculateConversion (convertInputOptionsToTxOutOptions op) extras
            in
                Just $
                    SellOfferT.mkSwapFTxADARedeemer
                        amount_FT
                        amount_ADA
                        (commission_ADA + sum_ONE_INVALID_NUMBER)
                        oracleData
                        (mkOracleDataSignature tp oracleData)
        consume_SellOffer_InvalidRedeemerType =
            Just SellOfferT.mkUpdateMinADARedeemer
        consume_SellOffer_InvalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        consume_SellOffer_UTxO_gen op extras =
            consume_TxOut_Gen
                input_SellOffer_UTxO_gen
                (consume_SellOffer_ValidRedeemerData op extras)
                (consume_SellOffer_InvalidRedeemerData op extras)
                consume_SellOffer_InvalidRedeemerType
                consume_SellOffer_InvalidRedeemerNonExist
                op
                extras
        -----------------
        validityRange_gen' op _ = validityRange_gen tp (tpTransactionDate tp) op
    in
        -----------------
        TxSpecs
            { tsInputsRef =
                [ (Protocol_TestEntity, input_Protocol_UTxO_gen)
                , (Fund_TestEntity, input_Fund_UTxO_gen)
                ]
            , tsInputsRefScripts = []
            , tsInputs =
                [
                    ( SellOffer_TestEntity
                    , consume_SellOffer_UTxO_gen
                    , SellOffer_SwapFTxADA_TestRedeemer
                    )
                ]
            , tsInputsFromWallet = []
            , tsOutputs = [(SellOffer_TestEntity, output_SellOffer_UTxO_gen)]
            , tsMints = []
            , tsUseSignatures = Nothing
            , tsUseValidityRange = Just validityRange_gen'
            , tsExtras =
                [ ("Valid FT Amount", True)
                , ("Invalid_Conversion", True)
                , ("Invalid_Commissions", True)
                , ("not isOrderOpen", False)
                , ("isOrderRestrictedForSellingADA", False)
                , ("isOrderRestrictedForSellingFT", False)
                , ("not isCorrect_Oracle_Signature", False)
                , ("Oracle Time Too Early", False)
                , ("Oracle Time Too Late", False)
                , ("FT Price ADA not found", False)
                , ("not isAmount_ADA_Available", True)
                ]
            }

--------------------------------------------------------------------------------

sellOffer_SwapADAxFT_TxSpecs :: TestParams -> [TxParam] -> TxSpecs
sellOffer_SwapADAxFT_TxSpecs tp !txParams =
    let
        -----------------
        token_FT_Price1xe6 = getTxParam "token_FT_Price1xe6" txParams :: Integer
        amount_ADA_fromUser = getTxParam "amount_ADA" txParams :: Integer
        available_FT = getTxParam "available_FT" txParams :: Integer
        -----------------
        oracleData = mkOracleData tp token_FT_Price1xe6 (tpTransactionDate tp)
        -----------------
        input_Protocol_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                (protocol_UTxO_MockData tp)
                Protocol_TestEntity
                op
        -----------------
        input_Fund_UTxO_gen op _ =
            txOut_With_TestEntity_Gen tp (fund_UTxO_MockData tp) Fund_TestEntity op
        -----------------
        -- Este es el generador. Puede generar zero, una o muchas txOuts, validas o invalidas.
        input_SellOffer_UTxO_gen op extras =
            -- aca el generador de txOut me genera una lista de txOuts segun test params
            let
                new_SellOffer_UTxO = sellOffer_UTxO_MockData_Parametrizable tp available_FT 5_000_000
                txOuts =
                    txOut_With_TestEntity_Gen
                        tp
                        new_SellOffer_UTxO
                        SellOffer_TestEntity
                        op
            in
                -- aca voy a seguir manipulando la lista de txOuts generadas en funcion de los extras
                case DataList.find P.snd extras of
                    -- cuando se usan extras, todos los test params son en valido
                    -- por lo tanto se que hay una sola y valida txOut
                    Just ("not isOrderOpen", _) ->
                        let
                            datum = SellOfferT.getSellOffer_DatumType_From_UTxO (head txOuts)
                            datumUpdated =
                                SellOffer.mkUpdated_SellOffer_Datum_With_StatusChanged
                                    datum
                                    T.sellOffer_Status_Closed
                        in
                            [ (head txOuts)
                                { LedgerApiV2.txOutDatum =
                                    LedgerApiV2.OutputDatum $
                                        SellOfferT.mkDatum datumUpdated
                                }
                            ]
                    Just ("isOrderRestrictedForSellingADA", True) ->
                        let
                            datum = SellOfferT.getSellOffer_DatumType_From_UTxO (head txOuts)
                            datumUpdated =
                                SellOffer.mkUpdated_SellOffer_Datum_With_RestrictionsChanged
                                    datum
                                    T.sellOffer_AllowSell
                                    T.sellOffer_NotAllowSell
                        in
                            [ (head txOuts)
                                { LedgerApiV2.txOutDatum =
                                    LedgerApiV2.OutputDatum $
                                        SellOfferT.mkDatum datumUpdated
                                }
                            ]
                    Just ("isOrderRestrictedForSellingFT", True) ->
                        let
                            datum = SellOfferT.getSellOffer_DatumType_From_UTxO (head txOuts)
                            datumUpdated =
                                SellOffer.mkUpdated_SellOffer_Datum_With_RestrictionsChanged
                                    datum
                                    T.sellOffer_NotAllowSell
                                    T.sellOffer_AllowSell
                        in
                            [ (head txOuts)
                                { LedgerApiV2.txOutDatum =
                                    LedgerApiV2.OutputDatum $
                                        SellOfferT.mkDatum datumUpdated
                                }
                            ]
                    Just ("not isAmount_FT_Available", True) -> txOuts -- deberia generar el valor incorrecto el property en las variables price and available ADA
                    _ -> txOuts
        -----------------
        -- Este se usa para calcular los outputs txOuts. Se necesita al menos una entrada para hacer eso
        input_SellOffer_UTxO_gen_for_calcs op extras =
            case op of
                TxOutInvalid (TxOutInvalidEntity (TxOutInvalidEntityDatum InvalidEntityDatumType)) -> sellOffer_UTxO_MockData tp
                TxOutInvalid (TxOutInvalidEntity (TxOutInvalidEntityDatum InvalidEntityDatumNonExist)) -> sellOffer_UTxO_MockData tp
                TxOutInvalid TxOutInvalidNone -> sellOffer_UTxO_MockData tp
                _ -> case input_SellOffer_UTxO_gen op extras of
                    []      -> sellOffer_UTxO_MockData tp
                    (x : _) -> x
        -----------------
        -- Este se usa para calculos internos. Se necesita al menos un datum de entrada para poder hacerlos
        input_SellOffer_Datum' op extras = SellOfferT.getSellOffer_DatumType_From_UTxO (input_SellOffer_UTxO_gen_for_calcs op extras)
        -----------------
        calculateConversion op extras =
            -----------------
            let
                input_SellOffer_Datum = input_SellOffer_Datum' op extras
                -----------------
                (_, _, price_FT_in_ADA) =
                    head $ T.iuValues $ T.odFTPriceADA1xe6 oracleData
                -----------------
                amount_FT = OnChainHelpers.divide_By_Scaled_1e6_And_RoundDownSafe amount_ADA_fromUser price_FT_in_ADA
                amount_ADA_real = OnChainHelpers.multiply_By_Scaled_1e6_And_RoundUp amount_FT price_FT_in_ADA
                --------
                commission = T.sodAskedCommission_InBPx1e3 input_SellOffer_Datum
                commission_FT = OnChainHelpers.multiply_By_Scaled_BPx1e3_And_RoundUp amount_FT commission
            in
                -----------------

                case DataList.find P.snd extras of
                    Just ("Invalid_Conversion", _) ->
                        let
                            invalidAmount = getTxParam "invalidAmount" txParams :: Integer
                        in
                            (amount_ADA_real, amount_FT + invalidAmount, commission_FT)
                    Just ("Invalid_Commissions", _) ->
                        let
                            invalidAmount = getTxParam "invalidAmount" txParams :: Integer
                        in
                            (amount_ADA_real, amount_FT, commission_FT + invalidAmount)
                    _ -> (amount_ADA_real, amount_FT, commission_FT)
        -----------------
        output_SellOffer_Datum op extras =
            -----------------
            let
                input_SellOffer_Datum = input_SellOffer_Datum' op extras
                -----------------
                (amount_ADA_real, amount_FT, commission_FT) = calculateConversion op extras
            in
                -----------------
                SellOffer.mkUpdated_SellOffer_Datum_With_SwapADAxFT
                    input_SellOffer_Datum
                    amount_ADA_real
                    amount_FT
                    commission_FT
        -----------------
        output_SellOffer_UTxO op extras =
            let
                newDatum = output_SellOffer_Datum op extras
                origValue = LedgerApiV2.txOutValue $ input_SellOffer_UTxO_gen_for_calcs op extras
                newAmount_ADA =
                    SellOfferT.sodAmount_ADA_Available newDatum
                        + SellOfferT.sodMinADA newDatum
                newAmount_FT = SellOfferT.sodAmount_FT_Available newDatum
                -----------------
                adjustedValue_ADA =
                    changeValue_Amount
                        origValue
                        OnChainHelpers.adaAssetClass
                        newAmount_ADA
                adjustedValue_FT =
                    changeValue_Amount
                        adjustedValue_ADA
                        (LedgerValue.assetClass (tpFundPolicy_CS tp) (tpFundFT_TN tp))
                        newAmount_FT
            in
                (sellOffer_UTxO_MockData tp)
                    { LedgerApiV2.txOutDatum =
                        LedgerApiV2.OutputDatum $ SellOfferT.mkDatum newDatum
                    , LedgerApiV2.txOutValue = adjustedValue_FT
                    }
        -----------------
        output_SellOffer_UTxO_gen op extras =
            txOut_With_TestEntity_Gen
                tp
                (output_SellOffer_UTxO op extras)
                SellOffer_TestEntity
                op
        -----------------
        consume_SellOffer_ValidRedeemerData op extras =
            let
                (amount_ADA_real, amount_FT, commission_FT) = calculateConversion (convertInputOptionsToTxOutOptions op) extras
                -- modifico oracleTokenFTData: el valor de la fecha del oraculo y otras cosas segun los extras
                oracleData' = case DataList.find P.snd extras of
                    Just ("Oracle Time Too Early", _) ->
                        let
                            -- Create the valid range based on the transaction date
                            validRange = createValidRange (tpTransactionDate tp)
                            -- Extract the lower limit from the valid range
                            lowerLimit = case Ledger.ivFrom validRange of
                                Ledger.LowerBound (Ledger.Finite a) True -> a
                                _                                        -> traceError "Invalid interval lower bound"
                            -- Adjust the lower limit by subtracting the valid time
                            newLowerLimitValue = lowerLimit - T.oracleData_Valid_Time
                            -- Set the oracle time to be just before the new lower limit
                            oracleTimeTooEarly = newLowerLimitValue - sum_ONE_INVALID_DATE
                        in
                            -- Create the oracle data with the adjusted time
                            mkOracleData tp token_FT_Price1xe6 oracleTimeTooEarly
                    Just ("Oracle Time Too Late", _) ->
                        let
                            -- Create the valid range based on the transaction date
                            validRange = createValidRange (tpTransactionDate tp)
                            -- Extract the upper limit from the valid range
                            upperLimit = case Ledger.ivTo validRange of
                                Ledger.UpperBound (Ledger.Finite a) _ -> a
                                _                                     -> traceError "Invalid interval upper bound"
                            -- Set the oracle time to be just after the upper limit
                            oracleTimeTooLate = upperLimit + sum_ONE_INVALID_DATE
                        in
                            -- Create the oracle data with the adjusted time
                            mkOracleData tp token_FT_Price1xe6 oracleTimeTooLate
                    Just ("FT Price ADA not found", True) ->
                        let
                            tokenFTPrice1xe6' =
                                T.InvestUnit
                                    -- cambio la poliza de FundID por cualquier otra, la de holding
                                    [ (tpFundHoldingPolicyID_CS tp, tpFundFT_TN tp, token_FT_Price1xe6)
                                    ]
                        in
                            T.Oracle_Data tokenFTPrice1xe6' (tpTransactionDate tp)
                    _ -> oracleData
                -- modifico oracleDataSignature segun los extras
                oracleDataSignature' =
                    case DataList.find P.snd extras of
                        Just ("not isCorrect_Oracle_Signature", _) ->
                            let
                                oracleTokenFTData_Changed = mkOracleData tp token_FT_Price1xe6 (tpTransactionDate tp + sum_ONE_INVALID_DATE)
                            in
                                mkOracleDataSignature tp oracleTokenFTData_Changed
                        _ -> mkOracleDataSignature tp oracleData'
            in
                SellOfferT.mkSwapADAxFTRedeemer
                    amount_ADA_real
                    amount_FT
                    commission_FT
                    oracleData'
                    oracleDataSignature'
        consume_SellOffer_InvalidRedeemerData op extras =
            let
                (amount_ADA_real, amount_FT, commission_FT) = calculateConversion (convertInputOptionsToTxOutOptions op) extras
            in
                Just $
                    SellOfferT.mkSwapADAxFTRedeemer
                        amount_ADA_real
                        amount_FT
                        (commission_FT + sum_ONE_INVALID_NUMBER)
                        oracleData
                        (mkOracleDataSignature tp oracleData)
        consume_SellOffer_InvalidRedeemerType =
            Just SellOfferT.mkUpdateMinADARedeemer
        consume_SellOffer_InvalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        consume_SellOffer_UTxO_gen op extras =
            consume_TxOut_Gen
                input_SellOffer_UTxO_gen
                (consume_SellOffer_ValidRedeemerData op extras)
                (consume_SellOffer_InvalidRedeemerData op extras)
                consume_SellOffer_InvalidRedeemerType
                consume_SellOffer_InvalidRedeemerNonExist
                op
                extras
        -----------------
        validityRange_gen' op _ = validityRange_gen tp (tpTransactionDate tp) op
    in
        -----------------
        TxSpecs
            { tsInputsRef =
                [ (Protocol_TestEntity, input_Protocol_UTxO_gen)
                , (Fund_TestEntity, input_Fund_UTxO_gen)
                ]
            , tsInputsRefScripts = []
            , tsInputs =
                [
                    ( SellOffer_TestEntity
                    , consume_SellOffer_UTxO_gen
                    , SellOffer_SwapFTxADA_TestRedeemer
                    )
                ]
            , tsInputsFromWallet = []
            , tsOutputs = [(SellOffer_TestEntity, output_SellOffer_UTxO_gen)]
            , tsMints = []
            , tsUseSignatures = Nothing
            , tsUseValidityRange = Just validityRange_gen'
            , tsExtras =
                [ ("Valid ADA Amount", True)
                , ("Invalid_Conversion", True)
                , ("Invalid_Commissions", True)
                , ("not isOrderOpen", False)
                , ("isOrderRestrictedForSellingADA", False)
                , ("isOrderRestrictedForSellingFT", False)
                , ("not isCorrect_Oracle_Signature", False)
                , ("Oracle Time Too Early", False)
                , ("Oracle Time Too Late", False)
                , ("FT Price ADA not found", False)
                , ("not isAmount_FT_Available", True)
                ]
            }

--------------------------------------------------------------------------------

sellOffer_Emergency_TxSpecs :: TestParams -> TxSpecs
sellOffer_Emergency_TxSpecs tp =
    -----------------
    let
        input_Fund_UTxO_gen op _ =
            txOut_With_TestEntity_Gen tp (fund_UTxO_MockData tp) Fund_TestEntity op
        -----------------
        input_SellOffer_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                (sellOffer_UTxO_MockData tp)
                SellOffer_TestEntity
                op
        -----------------
        input_SellOffer_Datum = SellOfferT.getSellOffer_DatumType_From_UTxO (sellOffer_UTxO_MockData tp)
        -----------------
        sellFT = T.sellOffer_NotAllowSell
        sellADA = T.sellOffer_NotAllowSell
        -----------------
        output_SellOffer_Datum =
            SellOffer.mkUpdated_SellOffer_Datum_With_RestrictionsChanged
                input_SellOffer_Datum
                sellFT
                sellADA
        -----------------
        output_SellOffer_UTxO =
            (sellOffer_UTxO_MockData tp)
                { LedgerApiV2.txOutDatum =
                    LedgerApiV2.OutputDatum $
                        SellOfferT.mkDatum output_SellOffer_Datum
                }
        -----------------
        output_SellOffer_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                output_SellOffer_UTxO
                SellOffer_TestEntity
                op
        -----------------
        consume_SellOffer_ValidRedeemerData =
            SellOfferT.mkUpdateSellRestrictionsRedeemer sellFT sellADA
        consume_SellOffer_InvalidRedeemerData =
            Just
                ( SellOfferT.mkUpdateSellRestrictionsRedeemer
                    T.sellOffer_AllowSell
                    T.sellOffer_NotAllowSell
                )
        consume_SellOffer_InvalidRedeemerType =
            Just SellOfferT.mkEmergencyRedeemer
        consume_SellOffer_InvalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        consume_SellOffer_UTxO_gen =
            consume_TxOut_Gen
                input_SellOffer_UTxO_gen
                consume_SellOffer_ValidRedeemerData
                consume_SellOffer_InvalidRedeemerData
                consume_SellOffer_InvalidRedeemerType
                consume_SellOffer_InvalidRedeemerNonExist
        -----------------
        signatures_gen' op _ = signatures_gen tp [tpSellOfferAdmin tp] op
        -----------------
        validityRange_gen' op _ = validityRange_gen tp (tpTransactionDate tp) op
    in
        -----------------
        TxSpecs
            { tsInputsRef = [(Fund_TestEntity, input_Fund_UTxO_gen)]
            , tsInputsRefScripts = []
            , tsInputs =
                [
                    ( SellOffer_TestEntity
                    , consume_SellOffer_UTxO_gen
                    , SellOffer_UpdateStatus_TestRedeemer
                    )
                ]
            , tsInputsFromWallet = []
            , tsOutputs = [(SellOffer_TestEntity, output_SellOffer_UTxO_gen)]
            , tsMints = []
            , tsUseSignatures = Just signatures_gen'
            , tsUseValidityRange = Just validityRange_gen'
            , tsExtras = []
            }

--------------------------------------------------------------------------------
