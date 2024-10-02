--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3
{- |
Module      : TestUtils.Contracts.TxSpecs.InvestUnit
Description :
-}
module TestUtils.Contracts.TxSpecs.InvestUnit where

-- Non-IOG imports
import           Prelude                              as P

-- IOG imports
import qualified Plutus.V2.Ledger.Api                 as LedgerApiV2

-- Project imports
import qualified Generic.OnChainHelpers               as OnChainHelpers
import qualified Protocol.Fund.Holding.Types          as FundHoldingT
import qualified Protocol.InvestUnit.OnChain          as InvestUnit
import qualified Protocol.InvestUnit.Types            as InvestUnitT
import qualified Protocol.Types                       as T
import           TestUtils.Automatic.ContextGenerator
import           TestUtils.Automatic.HelpersMAYZ
import           TestUtils.Automatic.Types
import           TestUtils.Constants
import           TestUtils.Contracts.InitialData
import           TestUtils.Helpers
import           TestUtils.Types
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------
-- InvestUnit Contract
--------------------------------------------------------------------------------

investUnit_ReIndexing_TxSpecs :: TestParams -> [TxParam] -> TxSpecs
investUnit_ReIndexing_TxSpecs tp txParams =
     let
        -----------------
        input_Protocol_UTxO_gen op _ =
            txOut_With_TestEntity_Gen tp (protocol_UTxO_MockData tp) Protocol_TestEntity op
        input_Fund_UTxO_gen op _ =
            txOut_With_TestEntity_Gen tp (fund_UTxO_With_Added_FundHolding_MockData tp) Fund_TestEntity op
        input_FundHolding_UTxO_gen op _ =
            txOut_With_TestEntity_Gen tp (fundHolding_UTxO_With_Deposits_MockData tp) FundHolding_TestEntity op
        input_InvestUnit_UTxO_gen op _ =
            txOut_With_TestEntity_Gen tp (investUnit_UTxO_MockData tp) InvestUnit_TestEntity op
         -----------------
        output_FundHolding_UTxO_gen op _ =
            txOut_With_TestEntity_Gen tp (fundHolding_UTxO_After_Reidx_MockData tp investUnit_Initial investUnit_AfterReIdx) FundHolding_TestEntity op
        -----------------
        output_InvestUnit_UTxO_gen op _ =
            txOut_With_TestEntity_Gen tp (investUnit_UTxO_After_ReIdx_MockData tp) InvestUnit_TestEntity op
        -----------------
        consume_FundHolding_ValidRedeemerData = FundHoldingT.mkReIndexingRedeemer investUnit_AfterReIdx investUnit_Initial
        consume_FundHolding_InvalidRedeemerData = Just $ FundHoldingT.mkReIndexingRedeemer investUnit_AfterReIdx investUnit_AfterReIdx
        consume_FundHolding_InvalidRedeemerType = Just FundHoldingT.mkBurnIDRedeemer
        consume_FundHolding_InvalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        consume_FundHolding_UTxO_gen =
            consume_TxOut_Gen
                input_FundHolding_UTxO_gen
                consume_FundHolding_ValidRedeemerData
                consume_FundHolding_InvalidRedeemerData
                consume_FundHolding_InvalidRedeemerType
                consume_FundHolding_InvalidRedeemerNonExist
        -----------------
        consume_InvestUnit_ValidRedeemerData = InvestUnitT.mkReIndexingRedeemer investUnit_AfterReIdx investUnit_Initial (oracleReIdxData tp) (oracleReIdxSignature tp)
        consume_InvestUnit_InvalidRedeemerData =
            let
                investUnit_Initial_Tokens = T.iuValues investUnit_Initial
                (cs, tn, _amt) = head investUnit_Initial_Tokens
                investUnit_Initial_Edited = T.InvestUnit $ (cs, tn, _amt + sum_ONE_INVALID_NUMBER) : tail investUnit_Initial_Tokens
            in Just $ InvestUnitT.mkReIndexingRedeemer investUnit_Initial_Edited investUnit_Initial (oracleReIdxData tp) (oracleReIdxSignature tp)
        consume_InvestUnit_InvalidRedeemerType = Just InvestUnitT.mkUpdateMinADARedeemer
        consume_InvestUnit_InvalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        consume_InvestUnit_UTxO_gen =
            consume_TxOut_Gen
                input_InvestUnit_UTxO_gen
                consume_InvestUnit_ValidRedeemerData
                consume_InvestUnit_InvalidRedeemerData
                consume_InvestUnit_InvalidRedeemerType
                consume_InvestUnit_InvalidRedeemerNonExist
        -----------------
        signatures_gen' op _ = signatures_gen tp (tpFundAdmins tp) op
        -----------------
        validityRange_gen' op _ = validityRange_gen tp (tpReIdxDate tp) op
     in -----------------
        TxSpecs
            { tsInputsRef = [(Protocol_TestEntity, input_Protocol_UTxO_gen), (Fund_TestEntity, input_Fund_UTxO_gen)]
            , tsInputsRefScripts = [uTxOForValidatorAsReference tp (tpFundHoldingValidator tp), uTxOForValidatorAsReference tp (tpInvestUnitValidator tp)]
            , tsInputs =
                [   ( FundHolding_TestEntity
                    , consume_FundHolding_UTxO_gen
                    , FundHolding_ReIndexing_TestRedeemer
                    ),
                    ( InvestUnit_TestEntity
                    , consume_InvestUnit_UTxO_gen
                    , InvestUnit_ReIndexing_TestRedeemer
                    )
                ]
            , tsInputsFromWallet = []
            , tsOutputs = [(FundHolding_TestEntity, output_FundHolding_UTxO_gen), (InvestUnit_TestEntity, output_InvestUnit_UTxO_gen)]
            , tsMints = []
            , tsUseSignatures = Just signatures_gen'
            , tsUseValidityRange = Just validityRange_gen'
            , tsExtras = []
        }


--------------------------------------------------------------------------------

investUnit_UpdateMinADA_TxSpecs :: TestParams -> [TxParam] -> TxSpecs
investUnit_UpdateMinADA_TxSpecs tp txParams =
    let -----------------
        newMinADA = getTxParam "newMinADA" txParams :: Integer
        -----------------
        input_Fund_UTxO_gen op _ =
            txOut_With_TestEntity_Gen tp (fund_UTxO_With_Added_FundHolding_MockData tp) Fund_TestEntity op
        -----------------
        input_InvestUnit_UTxO = investUnit_UTxO_MockData tp
        input_InvestUnit_Datum = InvestUnitT.getInvestUnit_DatumType_From_UTxO input_InvestUnit_UTxO
        -----------------
        input_InvestUnit_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                input_InvestUnit_UTxO
                InvestUnit_TestEntity
                op
        -----------------
        investUnit_Updated_DatumType =
            InvestUnit.mkUpdated_InvestUnit_Datum_With_MinADAChanged
                input_InvestUnit_Datum
                newMinADA
        -----------------
        origValue = LedgerApiV2.txOutValue input_InvestUnit_UTxO
        newAmount = InvestUnitT.iudMinADA investUnit_Updated_DatumType
        -----------------
        investUnit_Updated_UTxO =
            input_InvestUnit_UTxO
                { LedgerApiV2.txOutDatum =
                    LedgerApiV2.OutputDatum $
                        InvestUnitT.mkDatum investUnit_Updated_DatumType
                , LedgerApiV2.txOutValue =
                    changeValue_Amount
                        origValue
                        OnChainHelpers.adaAssetClass
                        newAmount
                }
        -----------------
        output_InvestUnit_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                investUnit_Updated_UTxO
                InvestUnit_TestEntity
                op
        -----------------
        consume_InvestUnit_ValidRedeemerData = InvestUnitT.mkUpdateMinADARedeemer
        consume_InvestUnit_InvalidRedeemerData = Nothing
        consume_InvestUnit_InvalidRedeemerType = Just $ InvestUnitT.mkReIndexingRedeemer investUnit_AfterReIdx investUnit_Initial (oracleReIdxData tp) (oracleReIdxSignature tp)
        consume_InvestUnit_InvalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        consume_InvestUnit_UTxO_gen =
            consume_TxOut_Gen
                input_InvestUnit_UTxO_gen
                consume_InvestUnit_ValidRedeemerData
                consume_InvestUnit_InvalidRedeemerData
                consume_InvestUnit_InvalidRedeemerType
                consume_InvestUnit_InvalidRedeemerNonExist
        -----------------
        signatures_gen' op _ = signatures_gen tp (tpFundAdmins tp) op
        -----------------
        validityRange_gen' op _ = validityRange_gen tp (tpTransactionDate tp) op
     in -----------------
        TxSpecs
            { tsInputsRef = [(Fund_TestEntity, input_Fund_UTxO_gen)]
            , tsInputsRefScripts = []
            , tsInputs =
                [
                    ( InvestUnit_TestEntity
                    , consume_InvestUnit_UTxO_gen
                    , InvestUnit_UpdateMinADA_TestRedeemer
                    )
                ]
            , tsInputsFromWallet = []
            , tsOutputs = [(InvestUnit_TestEntity, output_InvestUnit_UTxO_gen)]
            , tsMints = []
            , tsUseSignatures = Just signatures_gen'
            , tsUseValidityRange = Just validityRange_gen'
            , tsExtras = [("Valid Min ADA positive", True), ("Invalid Min ADA negative", True)]
            }



--------------------------------------------------------------------------------
