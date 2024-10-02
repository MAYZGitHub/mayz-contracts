--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

{- |
Module      : Tests.UnitTests.Main
Description : Unit Tests.
-}
module Main where
--------------------------------------------------------------------------------3

-- Non-IOG imports
import qualified Data.Proxy                           as DataProxy
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
import           Contracts.SellOffer.MintingPolicy
import           Contracts.SellOffer.Validator   
import           TestTree.Commissions
import           TestTree.ScaleAndRunding
import           TestTree.Values
import           TestUtils.Helpers
import           TestUtils.HelpersMAYZ

--------------------------------------------------------------------------------3

main :: IO ()
main = do
    putStrLn "---------------"
    tp <- getTestParams "export/test/deploy.json"
    numTests <- getNumTestCases 100
    putStrLn "---------------"
    let testGroup =
            Tasty.testGroup "Unit Tests"
                [ Tasty.testGroup "Contracts Tests"
                    [ Tasty.testGroup "Protocol Tests"
                        [ protocol_Policy_Tests tp
                        , protocol_Validator_Tests tp
                        ]
                    , Tasty.testGroup "Fund Tests"
                        [ fund_Policy_Tests tp
                        , fund_Validator_Tests tp
                        ]
                    , Tasty.testGroup "FundHolding Tests"
                        [ fundHolding_Policy_Tests tp
                        , fundHolding_Validator_Tests tp
                        ]
                    , Tasty.testGroup "InvestUnit Tests"
                        [investUnit_Validator_Tests tp]
                    , Tasty.testGroup "SellOffer Tests"
                        [ sellOffer_Policy_Tests tp
                        , sellOffer_Validator_Tests tp
                        ]
                    ]
                , Tasty.testGroup "Helpers Tests"
                    [ Tasty.testGroup "Value Tests"
                        [value_Tests tp]
                    , Tasty.testGroup "Comission Tests"
                        [comission_Tests tp]
                    , Tasty.testGroup "Scale and Runding Tests"
                        [scale_And_Runding_Tests tp]
                    ]
                ]

    Tasty.defaultMainWithIngredients
        (Tasty.includingOptions [TastyOptions.Option (DataProxy.Proxy :: DataProxy.Proxy TastyQC.QuickCheckTests)] : Tasty.defaultIngredients)
        (Tasty.localOption (TastyQC.QuickCheckTests numTests) testGroup )
