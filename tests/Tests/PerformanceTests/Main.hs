--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

module Main where
--------------------------------------------------------------------------------3

-- Non-IOG imports

import qualified Data.Proxy             as DataProxy
import           Prelude
import qualified Test.Tasty             as Tasty
import qualified Test.Tasty.Options     as TastyOptions
import qualified Test.Tasty.QuickCheck  as TastyQC

-- Project imports
import           TestTree.PropertyTests
import           TestTree.UnitTests
import           TestUtils.Helpers
import           TestUtils.HelpersMAYZ

--------------------------------------------------------------------------------3

main :: IO ()
main = do
    putStrLn "---------------"
    tp <- getTestParams "export/test/deploy.json"
    numTests <- getNumTestCases 100
    putStrLn "---------------"
    let testGroup = Tasty.testGroup "Performance Tests" [unit_Tests tp, property_Tests tp]
    Tasty.defaultMainWithIngredients
        (Tasty.includingOptions [TastyOptions.Option (DataProxy.Proxy :: DataProxy.Proxy TastyQC.QuickCheckTests)] : Tasty.defaultIngredients)
        (Tasty.localOption (TastyQC.QuickCheckTests numTests) testGroup )

--------------------------------------------------------------------------------3
