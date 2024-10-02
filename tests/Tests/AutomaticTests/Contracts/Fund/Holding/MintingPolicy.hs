--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4
{- |
Module      : Fund.Holding.MintingPolicy
Description : Validation logic and tests related to the FundHolding
              minting policy.

This module defines the validation logic for the FundHolding's minting policy.

It includes multiple test cases to ensure the integrity and correctness of the
minting script.
-}
module Contracts.Fund.Holding.MintingPolicy where

--------------------------------------------------------------------------------

-- Non-IOG imports
--------------------------------------------------------------------------------

-- Non-IOG imports
import           Prelude                                 as P (const, show)
import qualified Test.Tasty                              as Tasty

-- IOG imports
import           PlutusTx.Prelude

-- Project imports
import           TestUtils.Automatic.TestCases
import           TestUtils.Automatic.TestConfigTree
import           TestUtils.Automatic.TestRules
import           TestUtils.Automatic.TxGenerators
import           TestUtils.Automatic.Types
import           TestUtils.Constants
import           TestUtils.Contracts.TxSpecs.FundHolding
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------3

fundHolding_Policy_Tests :: TestParams -> RuleTree -> Tasty.TestTree
fundHolding_Policy_Tests tp ruleTree =
    Tasty.testGroup
        "FundHolding Policy Tests"
        [
            fundHolding_Policy_Redeemer_MintID_Tests tp ruleTree,
            fundHolding_Policy_Redeemer_BurnID_Tests tp ruleTree
        ]

--------------------------------------------------------------------------------

fundHolding_Policy_Redeemer_MintID_Tests :: TestParams -> RuleTree -> Tasty.TestTree
fundHolding_Policy_Redeemer_MintID_Tests tp ruleTree =
   let
        ------------------------
        txName = show FundHolding_Create_Tx
        txSpecs = fundHolding_Create_TxSpecs tp
        defaultTestCaseParams = generateTestCaseParams txSpecs
        selectedRedeemer = RedeemerLogPolicy (Just FundHolding_MintID_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp txSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer txSpecs ruleTree redeemerTestConfigTree
        ------------------------
    in
        transaction_Tests_Gen tp selectedRedeemer txName (P.const txSpecs) [] [] updatedTestConfigTree defaultTestCaseParams


--------------------------------------------------------------------------------

fundHolding_Policy_Redeemer_BurnID_Tests :: TestParams -> RuleTree -> Tasty.TestTree
fundHolding_Policy_Redeemer_BurnID_Tests tp ruleTree =
    let
        ------------------------
        txName = show FundHolding_Delete_Tx
        txSpecs = fundHolding_Delete_TxSpecs tp
        defaultTestCaseParams = generateTestCaseParams txSpecs
        selectedRedeemer = RedeemerLogPolicy (Just FundHolding_BurnID_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp txSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer txSpecs ruleTree redeemerTestConfigTree
        ------------------------
    in
        transaction_Tests_Gen tp selectedRedeemer txName (P.const txSpecs) [] [] updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------
