--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4

{- |
Module      : Tests.UnitTests.Protocol.Validator
Description : Validation logic and unit tests related to the Protocol validator.

This module defines the validation logic for the Protocol's contract.

It includes multiple unit test cases to ensure the integrity and correctness of
the validator script.
-}
module Contracts.Protocol.Validator where

--------------------------------------------------------------------------------

-- Non-IOG imports
import           Prelude                              as P
import           Test.QuickCheck.Instances.ByteString ()
import qualified Test.Tasty                           as Tasty

-- IOG imports

-- Project imports
import qualified Protocol.Constants                   as T
import qualified Protocol.Protocol.Types              as ProtocolT
import           TestUtils.Automatic.ParamsGenerators
import           TestUtils.Automatic.TestCases
import           TestUtils.Automatic.TestConfigTree
import           TestUtils.Automatic.TestRules
import           TestUtils.Automatic.TxGenerators
import           TestUtils.Automatic.Types
import           TestUtils.Constants
import           TestUtils.Contracts.InitialData
import           TestUtils.Contracts.TxSpecs.Protocol
import           TestUtils.Types
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------

protocol_Validator_Tests :: TestParams -> RuleTree -> Tasty.TestTree
protocol_Validator_Tests tp ruleTree =
    Tasty.testGroup
        "Protocol Validator Tests"
        [ protocol_Validator_Redeemer_DatumUpdate_Tests tp ruleTree
        , protocol_Validator_Redeemer_UpdateMinADA_Tests tp ruleTree
        , protocol_Validator_Redeemer_Emergency_Tests tp ruleTree
        ]

--------------------------------------------------------------------------------

protocol_Validator_Redeemer_DatumUpdate_Tests :: TestParams -> RuleTree -> Tasty.TestTree
protocol_Validator_Redeemer_DatumUpdate_Tests tp ruleTree =
    let ------------------------
        txName = show Protocol_DatumUpdate_Tx
        txSpecs = protocol_DatumUpdate_TxSpecs tp
        defaultTestCaseParams = generateTestCaseParams txSpecs
        selectedRedeemer = RedeemerLogValidator (Just Protocol_DatumUpdate_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp txSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer txSpecs ruleTree redeemerTestConfigTree
     in
        transaction_Tests_Gen tp selectedRedeemer txName (P.const txSpecs) [] [] updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------

protocol_Validator_Redeemer_UpdateMinADA_Tests :: TestParams -> RuleTree -> Tasty.TestTree
protocol_Validator_Redeemer_UpdateMinADA_Tests tp ruleTree =
    let ------------------------
        txName = show Protocol_UpdateMinADA_Tx
        txSpecs = protocol_UpdateMinADA_TxSpecs tp
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
        selectedRedeemer = RedeemerLogValidator (Just Protocol_UpdateMinADA_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp defaultTxSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer defaultTxSpecs ruleTree redeemerTestConfigTree
     in ------------------------

        transaction_Tests_Gen tp selectedRedeemer txName txSpecs txParams_Default txParamsGenerators_List updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------

protocol_Validator_Redeemer_Emergency_Tests :: TestParams -> RuleTree -> Tasty.TestTree
protocol_Validator_Redeemer_Emergency_Tests tp _ =
    let ------------------------
        txName = show Protocol_Emergency_Tx
        txSpecs = protocol_UpdateMinADA_TxSpecs tp
       ------------------------
        txParams_Default =
                [
                    TxParam "newMinADA" (toAlter_minAda::Integer)
                ]
        ------------------------
        defaultTxSpecs = txSpecs txParams_Default
        selectedRedeemer = RedeemerLogValidator (Just Protocol_Emergency_TestRedeemer)
        ------------------------
     in
        adminTokens_Tests_Gen tp txName selectedRedeemer (P.const defaultTxSpecs) ProtocolT.mkEmergencyRedeemer (tpTokenAdminPolicy_CS tp) (tpTokenEmergencyAdminPolicy_CS tp) T.protocolTokenAdmin_TN T.protocolTokenEmergencyAdmin_TN True True

--------------------------------------------------------------------------------
