--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2
module Main where

-- Non-IOG imports
import qualified Data.Proxy            as DataProxy
import           Prelude
import qualified Test.Tasty            as Tasty
import qualified Test.Tasty.Options    as TastyOptions
import qualified Test.Tasty.QuickCheck as TastyQC

-- IOG imports

-- Project imports
import           Protocol.Transactions
import           TestUtils.Helpers
import           TestUtils.HelpersMAYZ

--------------------------------------------------------------------------------2

main :: IO ()
main = do
    testCompiledCodeScripts <- getTestCompiledCodeScripts "export/test/deploy.json"
    numTests <- getNumTestCases 100
    let testGroup =
            Tasty.testGroup
                "Transactions Tests"
                [ protocolTransactionsTests testCompiledCodeScripts
                , Tasty.testGroup
                    "Fund Transactions Tests"
                    [
                    ]
                , Tasty.testGroup
                    "FundHolding Transactions Tests"
                    [
                    ]
                , Tasty.testGroup "InvestUnit Transactions Tests" []
                , Tasty.testGroup "SellOffer Transactions Tests" []
                ]

    Tasty.defaultMainWithIngredients
        (Tasty.includingOptions [TastyOptions.Option (DataProxy.Proxy :: DataProxy.Proxy TastyQC.QuickCheckTests)] : Tasty.defaultIngredients)
        (Tasty.localOption (TastyQC.QuickCheckTests numTests) testGroup )
