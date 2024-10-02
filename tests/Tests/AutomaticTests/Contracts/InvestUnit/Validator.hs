--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2
{- |
Module      : InvestUnit.Validator
Description : Validation logic and tests related to the InvestUnit module.

This module defines the validation logic for the InvestUnit contracts.

It includes multiple test cases to ensure the integrity and correctness of the
validation scripts.
-}
module Contracts.InvestUnit.Validator where

--------------------------------------------------------------------------------

-- Non-IOG imports
import           Prelude                                as P
import qualified Test.Tasty                             as Tasty

-- IOG imports

-- Project imports

import qualified Protocol.Constants                     as T
import qualified Protocol.InvestUnit.Types              as InvestUnitT
import           TestUtils.Automatic.ParamsGenerators
import           TestUtils.Automatic.TestCases
import           TestUtils.Automatic.TestConfigTree
import           TestUtils.Automatic.TestRules
import           TestUtils.Automatic.TxGenerators
import           TestUtils.Automatic.Types
import           TestUtils.Constants
import           TestUtils.Contracts.InitialData
import           TestUtils.Contracts.TxSpecs.InvestUnit
import           TestUtils.Types
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------

investUnit_Validator_Tests :: TestParams -> RuleTree -> Tasty.TestTree
investUnit_Validator_Tests tp ruleTree =
    Tasty.testGroup
        "InvestUnit Validator Tests"
        [
            investUnit_Validator_Redeemer_ReIndexing_Tests tp ruleTree
            , investUnit_Validator_Redeemer_UpdateMinADA_Tests tp ruleTree
            , investUnit_Validator_Redeemer_Emergency_Tests tp ruleTree
        ]

--------------------------------------------------------------------------------

investUnit_Validator_Redeemer_ReIndexing_Tests :: TestParams -> RuleTree -> Tasty.TestTree
investUnit_Validator_Redeemer_ReIndexing_Tests tp ruleTree =
    let
        ------------------------
        txName = show Fund_ReIndexing_Tx
        txSpecs = investUnit_ReIndexing_TxSpecs tp
        ------------
        txParams_Default =
                [
                ]
        ------------
        txParamsGenerators_List =
            []
        ------------------------
        defaultTxSpecs = txSpecs txParams_Default
        defaultTestCaseParams = generateTestCaseParams defaultTxSpecs
        selectedRedeemer = RedeemerLogValidator (Just InvestUnit_ReIndexing_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp defaultTxSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer defaultTxSpecs ruleTree redeemerTestConfigTree
        ------------------------
    in
        transaction_Tests_Gen tp selectedRedeemer txName txSpecs txParams_Default txParamsGenerators_List updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------

investUnit_Validator_Redeemer_UpdateMinADA_Tests :: TestParams -> RuleTree -> Tasty.TestTree
investUnit_Validator_Redeemer_UpdateMinADA_Tests tp ruleTree =
    let ------------------------
        txName = show InvestUnit_UpdateMinADA_Tx
        txSpecs = investUnit_UpdateMinADA_TxSpecs tp
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
        selectedRedeemer = RedeemerLogValidator (Just InvestUnit_UpdateMinADA_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp defaultTxSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer defaultTxSpecs ruleTree redeemerTestConfigTree
     in ------------------------
        transaction_Tests_Gen tp selectedRedeemer txName txSpecs txParams_Default txParamsGenerators_List updatedTestConfigTree defaultTestCaseParams


--------------------------------------------------------------------------------

investUnit_Validator_Redeemer_Emergency_Tests :: TestParams -> RuleTree -> Tasty.TestTree
investUnit_Validator_Redeemer_Emergency_Tests tp _ =
    let ------------------------
        txName = show InvestUnit_Emergency_Tx
        txSpecs = investUnit_UpdateMinADA_TxSpecs tp
        ------------------------
        txParams_Default =
                [
                    TxParam "newMinADA" (toAlter_minAda::Integer)
                ]
        ------------------------
        defaultTxSpecs = txSpecs txParams_Default
        selectedRedeemer = RedeemerLogValidator (Just InvestUnit_Emergency_TestRedeemer)
        ------------------------
     in
        adminTokens_Tests_Gen tp txName selectedRedeemer (P.const defaultTxSpecs) InvestUnitT.mkEmergencyRedeemer (tpTokenAdminPolicy_CS tp) (tpTokenEmergencyAdminPolicy_CS tp) T.protocolTokenAdmin_TN T.protocolTokenEmergencyAdmin_TN True True

--------------------------------------------------------------------------------
