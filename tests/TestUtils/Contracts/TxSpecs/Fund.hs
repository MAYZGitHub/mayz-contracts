--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

{- |
Module      : TestUtils.Contracts.TxSpecs.Fund
Description :
-}
module TestUtils.Contracts.TxSpecs.Fund where

-- Non-IOG imports

-- IOG imports

-- Project imports
import           TestUtils.Automatic.Types
import           TestUtils.Contracts.TxSpecs.FundHolding
import           TestUtils.TypesMAYZ
import TestUtils.Types
import TestUtils.Automatic.HelpersMAYZ
import TestUtils.Contracts.InitialData
import TestUtils.Automatic.ContextGenerator
import Prelude
import qualified Protocol.Fund.Types as FundT
import TestUtils.Helpers
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Generic.OnChainHelpers as OnChainHelpers
import qualified Protocol.Fund.Helpers as Fund

--------------------------------------------------------------------------------
-- Fund Contract
--------------------------------------------------------------------------------

fund_Create_TxSpecs :: TestParams -> TxSpecs
fund_Create_TxSpecs tp =
    let -----------------
        input_Protocol_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                (protocol_UTxO_MockData tp)
                Protocol_TestEntity
                op
        -----------------
        output_Fund_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                (fund_UTxO_MockData tp)
                Fund_TestEntity
                op
        -----------------
        output_InvestUnit_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                (investUnit_UTxO_MockData tp)
                InvestUnit_TestEntity
                op
        -----------------
        mint_FundID_gen op _ =
            mint_Value_With_TestToken_Gen
                tp
                (FundID_TestToken, Fund_MintID_TestRedeemer) 1
                op
        -----------------
        mint_InvestUnitID_gen op _ =
            mint_Value_With_TestToken_Gen
                tp
                (InvestUnitID_TestToken, Fund_MintID_TestRedeemer) 1
                op
        -----------------
        validityRange_gen' op _ = validityRange_gen tp (tpTransactionDate tp) op
     in -----------------
        TxSpecs
            { tsInputsRef = [(Protocol_TestEntity, input_Protocol_UTxO_gen)]
            , tsInputsRefScripts = [uTxOForMintingAsReference tp (tpFundPolicy tp)]
            , tsInputs = []
            , tsInputsFromWallet = [fund_spend_UTxO_And_TxOutRef_MockData tp]
            , tsOutputs = [(Fund_TestEntity, output_Fund_UTxO_gen), (InvestUnit_TestEntity, output_InvestUnit_UTxO_gen)]
            , tsMints =
                [
                    ( FundID_TestToken
                    , mint_FundID_gen
                    , Fund_MintID_TestRedeemer
                    ),
                    ( InvestUnitID_TestToken
                    , mint_InvestUnitID_gen
                    , Fund_MintID_TestRedeemer
                    )
                ]
            , tsUseSignatures = Nothing
            , tsUseValidityRange = Just validityRange_gen'
            , tsExtras = []
            }

--------------------------------------------------------------------------------

fund_Delete_TxSpecs :: TestParams -> TxSpecs
fund_Delete_TxSpecs tp =
     -----------------
    let input_Fund_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                (fund_UTxO_MockData tp)
                Fund_TestEntity
                op
        -----------------
        consume_Fund_ValidRedeemerData = FundT.mkDeleteRedeemer
        consume_Fund_InvalidRedeemerData = Nothing
        consume_Fund_InvalidRedeemerType =
            Just FundT.mkUpdateMinADARedeemer
        consume_Fund_InvalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        consume_Fund_UTxO_gen =
            consume_TxOut_Gen
                input_Fund_UTxO_gen
                consume_Fund_ValidRedeemerData
                consume_Fund_InvalidRedeemerData
                consume_Fund_InvalidRedeemerType
                consume_Fund_InvalidRedeemerNonExist
        -----------------
        burn_FundID_gen op _ =
            mint_Value_With_TestToken_Gen
                tp
                (FundID_TestToken, Fund_BurnID_TestRedeemer) 1
                op
        -----------------
        burn_InvestUnitID_gen op _ =
            mint_Value_With_TestToken_Gen
                tp
                (InvestUnitID_TestToken, Fund_BurnID_TestRedeemer) 1
                op
        -----------------
        signatures_gen' op _ = signatures_gen tp (tpFundAdmins tp) op
        -----------------
        validityRange_gen' op _ = validityRange_gen tp (tpTransactionDate tp) op
     in -----------------
        TxSpecs
            { tsInputsRef = []
            , tsInputsRefScripts = [uTxOForMintingAsReference tp (tpFundPolicy tp), uTxOForValidatorAsReference tp (tpFundValidator tp)]
            , tsInputs =
                [
                    ( Fund_TestEntity
                    , consume_Fund_UTxO_gen
                    , Fund_Delete_TestRedeemer
                    )
                ]
            , tsInputsFromWallet = []
            , tsOutputs = []
            , tsMints =
                [
                    ( FundID_TestToken
                    , burn_FundID_gen
                    , Fund_BurnID_TestRedeemer
                    )
                    ,
                    ( InvestUnitID_TestToken
                    , burn_InvestUnitID_gen
                    , Fund_BurnID_TestRedeemer
                    )
                ]
            , tsUseSignatures = Just signatures_gen'
            , tsUseValidityRange = Just validityRange_gen'
            , tsExtras = []
            }

--------------------------------------------------------------------------------

fund_Deposit_TxSpecs :: TestParams -> [TxParam] -> TxSpecs
fund_Deposit_TxSpecs = fundHolding_Deposit_TxSpecs

--------------------------------------------------------------------------------

fund_Withdraw_TxSpecs :: TestParams -> [TxParam] -> TxSpecs
fund_Withdraw_TxSpecs = fundHolding_Withdraw_TxSpecs

--------------------------------------------------------------------------------

fund_DatumUpdate_TxSpecs :: TestParams -> TxSpecs
fund_DatumUpdate_TxSpecs tp =
    -----------------
    let
        input_Fund_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                (fund_UTxO_MockData tp)
                Fund_TestEntity
                op
        -----------------
        input_Fund_Datum = FundT.getFund_DatumType_From_UTxO (fund_UTxO_MockData tp)
        -----------------
        output_Fund_Datum =
                    let
                        _admins = FundT.fdAdmins input_Fund_Datum
                        tokenAdminPolicy_CS = FundT.fdTokenAdminPolicy_CS input_Fund_Datum
                        -------------------
                        admins_updated = []
                        -------------------
                    in
                        Fund.mkUpdated_Fund_Datum_With_NormalChanges
                            input_Fund_Datum
                            admins_updated
                            tokenAdminPolicy_CS
                         
        -----------------
        output_Fund_UTxO =
            (fund_UTxO_MockData tp)
                { LedgerApiV2.txOutDatum =
                    LedgerApiV2.OutputDatum $
                FundT.mkDatum output_Fund_Datum
        }
        -----------------
        output_Fund_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                output_Fund_UTxO
                Fund_TestEntity
                op
        -----------------
        consume_Fund_ValidRedeemerData =
            FundT.mkDatumUpdateRedeemer
        consume_Fund_InvalidRedeemerData =
            Nothing
        consume_Fund_InvalidRedeemerType =
            Just FundT.mkDeleteRedeemer
        consume_Fund_InvalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        consume_Fund_UTxO_gen =
            consume_TxOut_Gen
                input_Fund_UTxO_gen
                consume_Fund_ValidRedeemerData
                consume_Fund_InvalidRedeemerData
                consume_Fund_InvalidRedeemerType
                consume_Fund_InvalidRedeemerNonExist
        -----------------
        signatures_gen' op _ = signatures_gen tp (tpFundAdmins tp) op
        -----------------
        validityRange_gen' op _ = validityRange_gen tp (tpTransactionDate tp) op
     in -----------------
        TxSpecs
            { tsInputsRef = []
            , tsInputsRefScripts = []
            , tsInputs =
                [
                    ( Fund_TestEntity
                    , consume_Fund_UTxO_gen
                    , Fund_DatumUpdate_TestRedeemer
                    )
                ]
            , tsInputsFromWallet = []
            , tsOutputs = [(Fund_TestEntity, output_Fund_UTxO_gen)]
            , tsMints = []
            , tsUseSignatures = Just signatures_gen'
            , tsUseValidityRange = Just validityRange_gen'
            , tsExtras = []
            }


--------------------------------------------------------------------------------

fund_UpdateMinADA_TxSpecs :: TestParams -> [TxParam] -> TxSpecs
fund_UpdateMinADA_TxSpecs tp txParams =
    let -----------------
        newMinADA = getTxParam "newMinADA" txParams :: Integer
        -----------------
        input_Fund_UTxO = fund_UTxO_MockData tp
        input_Fund_Datum = FundT.getFund_DatumType_From_UTxO input_Fund_UTxO
        -----------------
        input_Fund_UTxO_gen op _ =
            txOut_With_TestEntity_Gen tp input_Fund_UTxO Fund_TestEntity op
        -----------------
        output_Fund_Datum =
            Fund.mkUpdated_Fund_Datum_With_MinADAChanged
                input_Fund_Datum
                newMinADA
        -----------------
        origValue = LedgerApiV2.txOutValue input_Fund_UTxO
        newAmount = FundT.fdMinADA output_Fund_Datum
        -----------------
        output_Fund_UTxO =
            input_Fund_UTxO
                { LedgerApiV2.txOutDatum =
                    LedgerApiV2.OutputDatum $
                        FundT.mkDatum output_Fund_Datum
                , LedgerApiV2.txOutValue =
                    changeValue_Amount
                        origValue
                        OnChainHelpers.adaAssetClass
                        newAmount
                }
        -----------------
        output_Fund_UTxO_gen op _ =
            txOut_With_TestEntity_Gen tp output_Fund_UTxO Fund_TestEntity op
        -----------------
        consume_Fund_ValidRedeemerData = FundT.mkUpdateMinADARedeemer
        consume_Fund_InvalidRedeemerData = Nothing
        consume_Fund_InvalidRedeemerType = Just FundT.mkDeleteRedeemer
        consume_Fund_InvalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        consume_Fund_UTxO_gen =
            consume_TxOut_Gen
                input_Fund_UTxO_gen
                consume_Fund_ValidRedeemerData
                consume_Fund_InvalidRedeemerData
                consume_Fund_InvalidRedeemerType
                consume_Fund_InvalidRedeemerNonExist
        -----------------
        signatures_gen' op _ = signatures_gen tp (tpFundAdmins tp) op
        -----------------
        validityRange_gen' op _ = validityRange_gen tp (tpTransactionDate tp) op
     in -----------------
        TxSpecs
            { tsInputsRef = []
            , tsInputsRefScripts = []
            , tsInputs =
                [
                    ( Fund_TestEntity
                    , consume_Fund_UTxO_gen
                    , Fund_UpdateMinADA_TestRedeemer
                    )
                ]
            , tsInputsFromWallet = []
            , tsOutputs = [(Fund_TestEntity, output_Fund_UTxO_gen)]
            , tsMints = []
            , tsUseSignatures = Just signatures_gen'
            , tsUseValidityRange = Just validityRange_gen'
            , tsExtras = [("Valid Min ADA positive", True), ("Invalid Min ADA negative", True)]
            }

--------------------------------------------------------------------------------

fund_FundHoldingAdd_TxSpecs :: TestParams -> TxSpecs
fund_FundHoldingAdd_TxSpecs = fundHolding_Create_TxSpecs

--------------------------------------------------------------------------------

fund_FundHoldingDelete_TxSpecs :: TestParams -> TxSpecs
fund_FundHoldingDelete_TxSpecs = fundHolding_Delete_TxSpecs

--------------------------------------------------------------------------------

fund_Finish_TxSpecs :: TestParams -> TxSpecs
fund_Finish_TxSpecs tp =
    -----------------
    let
        --------------------------
        closedAt = tpTransactionDate tp
        --------------------------
        input_Fund_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                (fund_UTxO_MockData tp)
                Fund_TestEntity
                op
        -----------------
        input_Fund_Datum = FundT.getFund_DatumType_From_UTxO (fund_UTxO_MockData tp)
        -----------------
        output_Fund_Datum = Fund.mkUpdated_Fund_Datum_With_ClosedAt input_Fund_Datum closedAt
        -----------------
        output_Fund_UTxO =
            (fund_UTxO_MockData tp)
                { LedgerApiV2.txOutDatum =
                    LedgerApiV2.OutputDatum $
                FundT.mkDatum output_Fund_Datum
        }
        -----------------
        output_Fund_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                output_Fund_UTxO
                Fund_TestEntity
                op
        -----------------
        consume_Fund_ValidRedeemerData =
            FundT.mkFinishRedeemer closedAt
        consume_Fund_InvalidRedeemerData =
            Just $ FundT.mkFinishRedeemer 0
        consume_Fund_InvalidRedeemerType =
            Just FundT.mkDeleteRedeemer
        consume_Fund_InvalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        consume_Fund_UTxO_gen =
            consume_TxOut_Gen
                input_Fund_UTxO_gen
                consume_Fund_ValidRedeemerData
                consume_Fund_InvalidRedeemerData
                consume_Fund_InvalidRedeemerType
                consume_Fund_InvalidRedeemerNonExist
        -----------------
        signatures_gen' op _ = signatures_gen tp (tpFundAdmins tp) op
        -----------------
        validityRange_gen' op _ = validityRange_gen tp (tpTransactionDate tp) op
     in -----------------
        TxSpecs
            { tsInputsRef = []
            , tsInputsRefScripts = []
            , tsInputs =
                [
                    ( Fund_TestEntity
                    , consume_Fund_UTxO_gen
                    , Fund_Finish_TestRedeemer
                    )
                ]
            , tsInputsFromWallet = []
            , tsOutputs = [(Fund_TestEntity, output_Fund_UTxO_gen)]
            , tsMints = []
            , tsUseSignatures = Just signatures_gen'
            , tsUseValidityRange = Just validityRange_gen'
            , tsExtras = []
            }



--------------------------------------------------------------------------------
