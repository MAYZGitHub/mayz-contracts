--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4

{- |
Module      : Contracts.Fund.Validator
Description : Validation logic and tests related to the Fund module.

This module defines the validation logic for the Fund contracts, including the
creation and management of FundHoldings.

It includes multiple test cases to ensure the integrity and correctness of the
validation scripts.
-}
module Contracts.Fund.Validator where

--------------------------------------------------------------------------------


-- Non-IOG imports
import           Prelude                              as P
import qualified Test.Tasty                           as Tasty

-- IOG imports

-- Project imports

import qualified Protocol.Constants                   as T
import qualified Protocol.Fund.Types                  as FundT
import           TestUtils.Automatic.ParamsGenerators
import           TestUtils.Automatic.TestCases
import           TestUtils.Automatic.TestConfigTree
import           TestUtils.Automatic.TestRules
import           TestUtils.Automatic.TxGenerators
import           TestUtils.Automatic.Types
import           TestUtils.Contracts.InitialData
import           TestUtils.Contracts.TxSpecs.Fund
import           TestUtils.TypesMAYZ
import TestUtils.Types
import           TestUtils.Constants

--------------------------------------------------------------------------------

fund_Validator_Tests :: TestParams -> RuleTree -> Tasty.TestTree
fund_Validator_Tests tp ruleTree =
    Tasty.testGroup
        "Fund Validator Tests"
        [ fund_Validator_Redeemer_DatumUpdate_Tests tp ruleTree
        , fund_Validator_Redeemer_UpdateMinADA_Tests tp ruleTree
        , fund_Validator_Redeemer_FundHoldingAdd_Tests tp ruleTree
        , fund_Validator_Redeemer_FundHoldingDelete_Tests tp ruleTree
        , fund_Validator_Redeemer_Finish_Tests tp ruleTree
        , fund_Validator_Redeemer_Emergency_Tests tp ruleTree
        , fund_Validator_Redeemer_Delete_Tests tp ruleTree
        ]

--------------------------------------------------------------------------------

fund_Validator_Redeemer_DatumUpdate_Tests :: TestParams -> RuleTree -> Tasty.TestTree
fund_Validator_Redeemer_DatumUpdate_Tests tp ruleTree =
    let
        ------------------------
        txName = show Fund_DatumUpdate_Tx
        txSpecs = fund_DatumUpdate_TxSpecs tp
        defaultTestCaseParams = generateTestCaseParams txSpecs
        selectedRedeemer = RedeemerLogValidator (Just Fund_DatumUpdate_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp txSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer txSpecs ruleTree redeemerTestConfigTree
        ------------------------
    in
        transaction_Tests_Gen tp selectedRedeemer txName (P.const txSpecs) [] [] updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------

fund_Validator_Redeemer_UpdateMinADA_Tests :: TestParams -> RuleTree -> Tasty.TestTree
fund_Validator_Redeemer_UpdateMinADA_Tests tp ruleTree =
    let ------------------------
        txName = show Fund_UpdateMinADA_Tx
        txSpecs = fund_UpdateMinADA_TxSpecs tp
        ------------
        txParams_Default =
                [
                    TxParam "newMinADA" (toAlter_minAda::Integer)
                ]
        ------------
        txParamsGenerators_Valid =
            TxParamGenerators
                [
                    intRangeParam "newMinADA" 3_000_000 100_000_000
                ]
        txParamsGenerators_Negative =
            TxParamGenerators
                [
                    negativeIntParam "newMinADA"
                ]
        txParamsGenerators_List =
            [("Valid Min ADA positive", txParamsGenerators_Valid), ("Invalid Min ADA negative", txParamsGenerators_Negative)]
        ------------------------
        defaultTxSpecs = txSpecs txParams_Default
        defaultTestCaseParams = generateTestCaseParams defaultTxSpecs
        selectedRedeemer = RedeemerLogValidator (Just Fund_UpdateMinADA_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp defaultTxSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer defaultTxSpecs ruleTree redeemerTestConfigTree
     in ------------------------
        transaction_Tests_Gen tp selectedRedeemer txName txSpecs txParams_Default txParamsGenerators_List updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------

fund_Validator_Redeemer_FundHoldingAdd_Tests :: TestParams -> RuleTree -> Tasty.TestTree
fund_Validator_Redeemer_FundHoldingAdd_Tests tp ruleTree =
    let
        ------------------------
        txName = show FundHolding_Create_Tx
        txSpecs = fund_FundHoldingAdd_TxSpecs tp
        defaultTestCaseParams = generateTestCaseParams txSpecs
        selectedRedeemer = RedeemerLogValidator (Just Fund_FundHoldingAdd_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp txSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer txSpecs ruleTree redeemerTestConfigTree
        ------------------------
    in
        transaction_Tests_Gen tp selectedRedeemer txName (P.const txSpecs) [] [] updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------

fund_Validator_Redeemer_FundHoldingDelete_Tests :: TestParams -> RuleTree -> Tasty.TestTree
fund_Validator_Redeemer_FundHoldingDelete_Tests tp ruleTree =
    let
        ------------------------
        txName = show FundHolding_Delete_Tx
        txSpecs = fund_FundHoldingDelete_TxSpecs tp
        defaultTestCaseParams = generateTestCaseParams txSpecs
        selectedRedeemer = RedeemerLogValidator (Just Fund_FundHoldingDelete_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp txSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer txSpecs ruleTree redeemerTestConfigTree
        ------------------------
    in
        transaction_Tests_Gen tp selectedRedeemer txName (P.const txSpecs) [] [] updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------

fund_Validator_Redeemer_Finish_Tests :: TestParams -> RuleTree -> Tasty.TestTree
fund_Validator_Redeemer_Finish_Tests tp ruleTree =
    let ------------------------
        txName = show Fund_Finish_Tx
        txSpecs = fund_Finish_TxSpecs tp
        defaultTestCaseParams = generateTestCaseParams txSpecs
        selectedRedeemer = RedeemerLogValidator (Just Fund_Finish_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp txSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer txSpecs ruleTree redeemerTestConfigTree
        ------------------------
    in
        transaction_Tests_Gen tp selectedRedeemer txName (P.const txSpecs) [] [] updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------

fund_Validator_Redeemer_Emergency_Tests :: TestParams -> RuleTree -> Tasty.TestTree
fund_Validator_Redeemer_Emergency_Tests tp _ =
    let ------------------------
        txName = show Fund_Emergency_Tx
        txSpecs = fund_UpdateMinADA_TxSpecs tp
        ------------------------
        txParams_Default =
                [
                    TxParam "newMinADA" (toAlter_minAda::Integer)
                ]
        ------------------------
        defaultTxSpecs = txSpecs txParams_Default
        selectedRedeemer = RedeemerLogValidator (Just Fund_Emergency_TestRedeemer)
        ------------------------
     in
        adminTokens_Tests_Gen tp txName selectedRedeemer (P.const defaultTxSpecs) FundT.mkEmergencyRedeemer (tpTokenAdminPolicy_CS tp) (tpTokenEmergencyAdminPolicy_CS tp) T.protocolTokenAdmin_TN T.protocolTokenEmergencyAdmin_TN True True

--------------------------------------------------------------------------------


fund_Validator_Redeemer_Delete_Tests :: TestParams -> RuleTree -> Tasty.TestTree
fund_Validator_Redeemer_Delete_Tests tp ruleTree =
    let
        ------------------------
        txName = show Fund_Delete_Tx
        txSpecs = fund_Delete_TxSpecs tp
        defaultTestCaseParams = generateTestCaseParams txSpecs
        selectedRedeemer = RedeemerLogValidator (Just Fund_Delete_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp txSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer txSpecs ruleTree redeemerTestConfigTree
        ------------------------
    in
        transaction_Tests_Gen tp selectedRedeemer txName (P.const txSpecs) [] [] updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------
