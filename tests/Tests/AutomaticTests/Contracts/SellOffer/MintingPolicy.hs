--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4
{- |
Module      : SellOffer.MintingPolicy
Description : Validation logic and tests related to the SellOffer minting policy.

This module defines the validation logic for the SellOffer's minting policy.

It includes multiple test cases to ensure the integrity and correctness of the
minting script.
-}
module Contracts.SellOffer.MintingPolicy where

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
import           TestUtils.Contracts.TxSpecs.SellOffer
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------

sellOffer_Policy_Tests :: TestParams -> RuleTree -> Tasty.TestTree
sellOffer_Policy_Tests tp ruleTree =
    Tasty.testGroup
        "SellOffer Policy Tests"

        [
            sellOffer_Policy_Redeemer_MintID_Tests tp ruleTree,
            sellOffer_Policy_Redeemer_BurnID_Tests tp ruleTree
        ]

--------------------------------------------------------------------------------

sellOffer_Policy_Redeemer_MintID_Tests :: TestParams -> RuleTree -> Tasty.TestTree
sellOffer_Policy_Redeemer_MintID_Tests tp ruleTree =
    let
        ------------------------
        txName = show SellOffer_Create_Tx
        txSpecs = sellOffer_Create_TxSpecs tp
        defaultTestCaseParams = generateTestCaseParams txSpecs
        selectedRedeemer = RedeemerLogPolicy (Just SellOffer_MintID_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp txSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer txSpecs ruleTree redeemerTestConfigTree
        ------------------------
    in
        transaction_Tests_Gen tp selectedRedeemer txName (P.const txSpecs) [] [] updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------

sellOffer_Policy_Redeemer_BurnID_Tests :: TestParams -> RuleTree -> Tasty.TestTree
sellOffer_Policy_Redeemer_BurnID_Tests tp ruleTree =
    let
        ------------------------
        txName = show SellOffer_Delete_Tx
        txSpecs = sellOffer_Delete_TxSpecs tp
        defaultTestCaseParams = generateTestCaseParams txSpecs
        selectedRedeemer = RedeemerLogPolicy (Just SellOffer_BurnID_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp txSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer txSpecs ruleTree redeemerTestConfigTree
        ------------------------
    in
        transaction_Tests_Gen tp selectedRedeemer txName (P.const txSpecs) [] [] updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------
