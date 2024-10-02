--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

{- |
Module      : Tests.AutomaticTests.Main
Description : Automatic Tests
-}
module Main where

--------------------------------------------------------------------------------3

-- Non-IOG imports
import           Data.Proxy                           as DataProxy (Proxy (..))
import           Prelude
import qualified Test.Tasty                           as Tasty
import qualified Test.Tasty.Options                   as TastyOptions
import qualified Test.Tasty.QuickCheck                as TastyQC

-- Project imports
import           Contracts.Fund.Holding.MintingPolicy
import           Contracts.Fund.Holding.Validator
import           Contracts.Fund.MintingPolicy
import           Contracts.Fund.Validator
import           Contracts.InvestUnit.Validator
import           Contracts.Protocol.MintingPolicy
import           Contracts.Protocol.Validator
import           Contracts.SwapOffer.MintingPolicy
import           Contracts.SwapOffer.Validator
import           TestUtils.Automatic.TestRules
import           TestUtils.Constants
import           TestUtils.Helpers
import           TestUtils.HelpersMAYZ

--------------------------------------------------------------------------------3

main :: IO ()
main = do
    putStrLn "---------------"
    tp <- getTestParams "export/test/deploy.json"
    ruleTree <- readRuleTreeFromExcel swTraceRuleTree "tests/config/tests.xlsx"
    numTests <- getNumTestCases 100
    putStrLn "---------------"
    let testGroup =
            Tasty.testGroup
                "Automatic Tests"
                [ Tasty.testGroup
                    "Contracts Tests"
                    [ Tasty.testGroup
                        "Protocol Tests"
                        [ protocol_Policy_Tests tp ruleTree
                        , protocol_Validator_Tests tp ruleTree
                        ]
                    , Tasty.testGroup
                        "Fund Tests"
                        [ fund_Policy_Tests tp ruleTree
                        , fund_Validator_Tests tp ruleTree
                        ]
                    , Tasty.testGroup
                        "FundHolding Tests"
                        [ fundHolding_Policy_Tests tp ruleTree
                        , fundHolding_Validator_Tests tp ruleTree
                        ]
                    , Tasty.testGroup
                        "InvestUnit Tests"
                        [investUnit_Validator_Tests tp ruleTree]
                    , Tasty.testGroup
                        "SwapOffer Tests"
                        [ swapOffer_Policy_Tests tp ruleTree
                        , swapOffer_Validator_Tests tp ruleTree
                        ]
                    ]
                ]
    Tasty.defaultMainWithIngredients
        (Tasty.includingOptions [TastyOptions.Option (Proxy :: DataProxy.Proxy TastyQC.QuickCheckTests)] : Tasty.defaultIngredients)
        (Tasty.localOption (TastyQC.QuickCheckTests numTests) testGroup)

--------------------------------------------------------------------------------3
