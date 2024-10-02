--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4
{- |
Module      : SwapOffer.MintingPolicy
Description : Validation logic and tests related to the SwapOffer minting policy.

This module defines the validation logic for the SwapOffer's minting policy.

It includes multiple test cases to ensure the integrity and correctness of the
minting script.
-}
module Contracts.SwapOffer.MintingPolicy where

--------------------------------------------------------------------------------

-- Non-IOG imports
import           Prelude                               as P
import qualified Test.Tasty                            as Tasty

-- IOG imports

-- Project imports
import           TestUtils.Automatic.TestCases
import           TestUtils.Automatic.TestConfigTree
import           TestUtils.Automatic.TestRules
import           TestUtils.Automatic.TxGenerators
import           TestUtils.Automatic.Types
import           TestUtils.Constants
import           TestUtils.Contracts.TxSpecs.SwapOffer
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------

swapOffer_Policy_Tests :: TestParams -> RuleTree -> Tasty.TestTree
swapOffer_Policy_Tests tp ruleTree =
    Tasty.testGroup
        "SwapOffer Policy Tests"

        [
            swapOffer_Policy_Redeemer_MintID_Tests tp ruleTree,
            swapOffer_Policy_Redeemer_BurnID_Tests tp ruleTree
        ]

--------------------------------------------------------------------------------

swapOffer_Policy_Redeemer_MintID_Tests :: TestParams -> RuleTree -> Tasty.TestTree
swapOffer_Policy_Redeemer_MintID_Tests tp ruleTree =
    let
        ------------------------
        txName = show SwapOffer_Create_Tx
        txSpecs = swapOffer_Create_TxSpecs tp
        defaultTestCaseParams = generateTestCaseParams txSpecs
        selectedRedeemer = RedeemerLogPolicy (Just SwapOffer_MintID_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp txSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer txSpecs ruleTree redeemerTestConfigTree
        ------------------------
    in
        transaction_Tests_Gen tp selectedRedeemer txName (P.const txSpecs) [] [] updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------

swapOffer_Policy_Redeemer_BurnID_Tests :: TestParams -> RuleTree -> Tasty.TestTree
swapOffer_Policy_Redeemer_BurnID_Tests tp ruleTree =
    let
        ------------------------
        txName = show SwapOffer_Delete_Tx
        txSpecs = swapOffer_Delete_TxSpecs tp
        defaultTestCaseParams = generateTestCaseParams txSpecs
        selectedRedeemer = RedeemerLogPolicy (Just SwapOffer_BurnID_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp txSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer txSpecs ruleTree redeemerTestConfigTree
        ------------------------
    in
        transaction_Tests_Gen tp selectedRedeemer txName (P.const txSpecs) [] [] updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------
