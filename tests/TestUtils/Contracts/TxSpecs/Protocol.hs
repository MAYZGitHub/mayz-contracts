--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

{- |
Module      : TestUtils.Contracts.TxSpecs.Protocol
Description :
-}
module TestUtils.Contracts.TxSpecs.Protocol where

-- Non-IOG imports

import qualified Prelude                              as P

-- IOG imports
import qualified Plutus.V2.Ledger.Api                 as LedgerApiV2
import           PlutusTx.Prelude

-- Project imports

import qualified Generic.OnChainHelpers               as OnChainHelpers
import qualified Protocol.Protocol.Helpers            as Protocol
import qualified Protocol.Protocol.Types              as ProtocolT
import           TestUtils.Automatic.ContextGenerator
import           TestUtils.Automatic.HelpersMAYZ
import           TestUtils.Automatic.ParamsGenerators
import           TestUtils.Automatic.Types
import           TestUtils.Contracts.InitialData
import           TestUtils.Helpers
import           TestUtils.TypesMAYZ
import TestUtils.Types

--------------------------------------------------------------------------------
-- Protocol Contract
--------------------------------------------------------------------------------

protocol_Create_TxSpecs :: TestParams -> TxSpecs
protocol_Create_TxSpecs tp =
    let -----------------
        input_Protocol_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                (protocol_UTxO_MockData tp)
                Protocol_TestEntity
                op
        -----------------
        mint_ProtocolID_gen op _ =
            mint_Value_With_TestToken_Gen
                tp
                (ProtocolID_TestToken, Protocol_MintID_TestRedeemer) 1
                op
        -----------------
        validityRange_gen' op _ = validityRange_gen tp (tpTransactionDate tp) op
     in -----------------
        TxSpecs
            { tsInputsRef = []
            , tsInputsRefScripts = []
            , tsInputs = []
            , tsInputsFromWallet = [protocol_spend_UTxO_And_TxOutRef_MockData tp]
            , tsOutputs = [(Protocol_TestEntity, input_Protocol_UTxO_gen)]
            , tsMints =
                [
                    ( ProtocolID_TestToken
                    , mint_ProtocolID_gen
                    , Protocol_MintID_TestRedeemer
                    )
                ]
            , tsUseSignatures = Nothing
            , tsUseValidityRange = Just validityRange_gen'
            , tsExtras = []
            }

--------------------------------------------------------------------------------

protocol_DatumUpdate_TxSpecs :: TestParams -> TxSpecs
protocol_DatumUpdate_TxSpecs tp =
    -----------------
    let
        input_Protocol_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                (protocol_UTxO_MockData tp)
                Protocol_TestEntity
                op
        -----------------
        input_Protocol_Datum = ProtocolT.getProtocol_DatumType_From_UTxO (protocol_UTxO_MockData tp)
        -----------------
        output_Protocol_Datum =
                    let
                        -- Extract values from the current protocol datum
                        -------------------
                        oraclePaymentPubKey = ProtocolT.pdOraclePaymentPubKey input_Protocol_Datum
                        _admins = ProtocolT.pdAdmins input_Protocol_Datum
                        tokenAdminPolicy_CS = ProtocolT.pdTokenAdminPolicy_CS input_Protocol_Datum
                        fundCategories = ProtocolT.pdFundCategories input_Protocol_Datum
                        fundLifeTime = ProtocolT.pdFundLifeTime input_Protocol_Datum
                        requiredMAYZForSellOffer = ProtocolT.pdRequiredMAYZForSellOffer input_Protocol_Datum
                        requiredMAYZForBuyOrder = ProtocolT.pdRequiredMAYZForBuyOrder input_Protocol_Datum
                        commissionFund_PerYear_InBPx1e3 = ProtocolT.pdCommissionFund_PerYear_InBPx1e3 input_Protocol_Datum
                        commissionSellOffer_InBPx1e3 = ProtocolT.pdCommissionSellOffer_InBPx1e3 input_Protocol_Datum
                        commissionBuyOrder_InBPx1e3 = ProtocolT.pdCommissionBuyOrder_InBPx1e3 input_Protocol_Datum
                        share_InBPx1e2_Protocol = ProtocolT.pdShare_InBPx1e2_Protocol input_Protocol_Datum
                        share_InBPx1e2_Delegators = ProtocolT.pdShare_InBPx1e2_Delegators input_Protocol_Datum
                        share_InBPx1e2_Managers = ProtocolT.pdShare_InBPx1e2_Managers input_Protocol_Datum
                        delegatorsAdmins = ProtocolT.pdDelegatorsAdmins input_Protocol_Datum
                        -------------------
                        admins_updated = []
                        -------------------
                    in
                        Protocol.mkUpdated_Protocol_Datum_With_NormalChanges
                            input_Protocol_Datum
                            oraclePaymentPubKey
                            admins_updated
                            tokenAdminPolicy_CS
                            fundCategories
                            fundLifeTime
                            requiredMAYZForSellOffer
                            requiredMAYZForBuyOrder
                            commissionFund_PerYear_InBPx1e3
                            commissionSellOffer_InBPx1e3
                            commissionBuyOrder_InBPx1e3
                            share_InBPx1e2_Protocol
                            share_InBPx1e2_Delegators
                            share_InBPx1e2_Managers
                            delegatorsAdmins
        -----------------
        output_Protocol_UTxO =
            (protocol_UTxO_MockData tp)
                { LedgerApiV2.txOutDatum =
                    LedgerApiV2.OutputDatum $
                ProtocolT.mkDatum output_Protocol_Datum
        }
        -----------------
        output_Protocol_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                output_Protocol_UTxO
                Protocol_TestEntity
                op
        -----------------
        consume_Protocol_ValidRedeemerData =
            ProtocolT.mkDatumUpdateRedeemer
        consume_Protocol_InvalidRedeemerData =
            Nothing
        consume_Protocol_InvalidRedeemerType =
            Just ProtocolT.mkUpdateMinADARedeemer
        consume_Protocol_InvalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        consume_Protocol_UTxO_gen =
            consume_TxOut_Gen
                input_Protocol_UTxO_gen
                consume_Protocol_ValidRedeemerData
                consume_Protocol_InvalidRedeemerData
                consume_Protocol_InvalidRedeemerType
                consume_Protocol_InvalidRedeemerNonExist
        -----------------
        signatures_gen' op _ = signatures_gen tp (tpProtocolAdmins tp) op
        -----------------
        validityRange_gen' op _ = validityRange_gen tp (tpTransactionDate tp) op
     in -----------------
        TxSpecs
            { tsInputsRef = []
            , tsInputsRefScripts = []
            , tsInputs =
                [
                    ( Protocol_TestEntity
                    , consume_Protocol_UTxO_gen
                    , Protocol_DatumUpdate_TestRedeemer
                    )
                ]
            , tsInputsFromWallet = []
            , tsOutputs = [(Protocol_TestEntity, output_Protocol_UTxO_gen)]
            , tsMints = []
            , tsUseSignatures = Just signatures_gen'
            , tsUseValidityRange = Just validityRange_gen'
            , tsExtras = []
            }

--------------------------------------------------------------------------------

protocol_UpdateMinADA_TxSpecs :: TestParams -> [TxParam] -> TxSpecs
protocol_UpdateMinADA_TxSpecs tp txParams =
    let -----------------
        newMinADA = getTxParam "newMinADA" txParams :: Integer
        -----------------
        input_Protocol_UTxO = protocol_UTxO_MockData tp
        input_Protocol_Datum = ProtocolT.getProtocol_DatumType_From_UTxO input_Protocol_UTxO
        -----------------
        input_Protocol_UTxO_gen op _ =
            txOut_With_TestEntity_Gen tp input_Protocol_UTxO Protocol_TestEntity op
        -----------------
        output_Protocol_Datum =
            Protocol.mkUpdated_Protocol_Datum_With_MinADAChanged
                input_Protocol_Datum
                newMinADA
        -----------------
        origValue = LedgerApiV2.txOutValue input_Protocol_UTxO
        newAmount = ProtocolT.pdMinADA output_Protocol_Datum
        -----------------
        output_Protocol_UTxO =
            input_Protocol_UTxO
                { LedgerApiV2.txOutDatum =
                    LedgerApiV2.OutputDatum $
                        ProtocolT.mkDatum output_Protocol_Datum
                , LedgerApiV2.txOutValue =
                    changeValue_Amount
                        origValue
                        OnChainHelpers.adaAssetClass
                        newAmount
                }
        -----------------
        output_Protocol_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                output_Protocol_UTxO
                Protocol_TestEntity
                op
        -----------------
        consume_Protocol_ValidRedeemerData = ProtocolT.mkUpdateMinADARedeemer
        consume_Protocol_InvalidRedeemerData = Nothing
        consume_Protocol_InvalidRedeemerType = Just ProtocolT.mkDatumUpdateRedeemer
        consume_Protocol_InvalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        consume_Protocol_UTxO_gen =
            consume_TxOut_Gen
                input_Protocol_UTxO_gen
                consume_Protocol_ValidRedeemerData
                consume_Protocol_InvalidRedeemerData
                consume_Protocol_InvalidRedeemerType
                consume_Protocol_InvalidRedeemerNonExist
        -----------------
        signatures_gen' op _ = signatures_gen tp (tpProtocolAdmins tp) op
        -----------------
        validityRange_gen' op _ = validityRange_gen tp (tpTransactionDate tp) op
     in -----------------
        TxSpecs
            { tsInputsRef = []
            , tsInputsRefScripts = []
            , tsInputs =
                [
                    ( Protocol_TestEntity
                    , consume_Protocol_UTxO_gen
                    , Protocol_UpdateMinADA_TestRedeemer
                    )
                ]
            , tsInputsFromWallet = []
            , tsOutputs = [(Protocol_TestEntity, output_Protocol_UTxO_gen)]
            , tsMints = []
            , tsUseSignatures = Just signatures_gen'
            , tsUseValidityRange = Just validityRange_gen'
            , tsExtras = [("Valid Min ADA positive", True), ("Invalid Min ADA negative", True)]
            }

--------------------------------------------------------------------------------
