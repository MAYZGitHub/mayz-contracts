--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4

module Contracts.Protocol.MintingPolicy where

--------------------------------------------------------------------------------

-- Non-IOG imports
import           Prelude                              as P
import qualified Test.Tasty                           as Tasty

-- IOG imports

-- Project imports
import           TestUtils.Automatic.TestCases
import           TestUtils.Automatic.TestConfigTree
import           TestUtils.Automatic.TestRules
import           TestUtils.Automatic.TxGenerators
import           TestUtils.Automatic.Types
import           TestUtils.Constants
import           TestUtils.Contracts.TxSpecs.Protocol
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------

protocol_Policy_Tests :: TestParams -> RuleTree -> Tasty.TestTree
protocol_Policy_Tests tp ruleTree =
    Tasty.testGroup
        "Protocol Policy Tests"
        [
            protocol_Policy_Redeemer_MintID_Tests tp ruleTree
        ]

--------------------------------------------------------------------------------

protocol_Policy_Redeemer_MintID_Tests :: TestParams -> RuleTree -> Tasty.TestTree
protocol_Policy_Redeemer_MintID_Tests tp ruleTree =
    let
        ------------------------
        txName = show Protocol_Create_Tx
        txSpecs = protocol_Create_TxSpecs tp
        defaultTestCaseParams = generateTestCaseParams txSpecs
        selectedRedeemer = RedeemerLogPolicy (Just Protocol_MintID_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp txSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer txSpecs ruleTree redeemerTestConfigTree
        ------------------------
    in
        transaction_Tests_Gen tp selectedRedeemer txName (P.const txSpecs) [] [] updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------
