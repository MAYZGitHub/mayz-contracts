--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4

module TestTree.ScaleAndRunding where
--------------------------------------------------------------------------------
-- Non-IOG imports
import qualified Data.List              as DataList
import           Prelude                hiding (length)
import qualified Prelude                as P
import qualified Test.Tasty             as Tasty
import qualified Test.Tasty.HUnit       as Tasty

-- IOG imports

-- Project imports
import qualified Generic.OnChainHelpers as OnChainHelpers
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------

-- Helper function for detailed logging
logDetails :: String -> [(String, Integer)] -> String
logDetails opName values = DataList.intercalate "\n" $
    ("Operation: " ++ opName) : map (\(name, val) -> name ++ ": " ++ show val) values

--------------------------------------------------------------------------------

scale_And_Runding_Tests :: TestParams -> Tasty.TestTree
scale_And_Runding_Tests _tp =
    Tasty.testGroup "Tests"
    [
        Tasty.testCase "Multiply 100 by 1.05 and round up" $
            OnChainHelpers.multiply_By_Scaled_1e6_And_RoundUp 100 1050000 Tasty.@?= 105
        , Tasty.testCase "Multiply 100 by 1.049999 and round up" $
            OnChainHelpers.multiply_By_Scaled_1e6_And_RoundUp 100 1049999 Tasty.@?= 105
        , Tasty.testCase "Multiply 100 by 1.05 and round down" $
            OnChainHelpers.multiply_By_Scaled_1e6_And_RoundDown 100 1050000 Tasty.@?= 105
        , Tasty.testCase "Multiply 100 by 1.049999 and round down" $
            OnChainHelpers.multiply_By_Scaled_1e6_And_RoundDown 100 1049999 Tasty.@?= 104
        , Tasty.testCase "Divide 105 by 1 safely" $
            OnChainHelpers.divide_By_Scaled_1e6_And_RoundDownSafe 105 1000000 Tasty.@?= 105
        , Tasty.testCase "Divide 105 by 1.05 safely" $
            OnChainHelpers.divide_By_Scaled_1e6_And_RoundDownSafe 105 1050000 Tasty.@?= 100
        , Tasty.testCase "Multiply 1000000 by 0.000001 and round up" $
            OnChainHelpers.multiply_By_Scaled_1e6_And_RoundUp 1000000 1 Tasty.@?= 1
        , Tasty.testCase "Divide 1 by 0.000001 safely" $
            OnChainHelpers.divide_By_Scaled_1e6_And_RoundDownSafe 1 1 Tasty.@?= 1000000
        , Tasty.testCase "Divide 1000001 by 1 safely" $
            OnChainHelpers.divide_By_Scaled_1e6_And_RoundDownSafe 1000001 1000000 Tasty.@?= 1000001
        , Tasty.testCase "Multiply 1 by 1 and round up" $
            OnChainHelpers.multiply_By_Scaled_1e6_And_RoundUp 1 1 Tasty.@?= 1
        , Tasty.testCase "Multiply max integer by 1 and round up" $
            OnChainHelpers.multiply_By_Scaled_1e6_And_RoundUp (fromIntegral (P.maxBound :: Int)) 1000000 Tasty.@?= fromIntegral (P.maxBound :: Int)
        , Tasty.testCase "Divide 1 by max integer safely" $
            OnChainHelpers.divide_By_Scaled_1e6_And_RoundDownSafe 1 (fromIntegral (P.maxBound :: Int)) Tasty.@?= 0
        , Tasty.testCase "Multiply 0 by any number and round up" $
            OnChainHelpers.multiply_By_Scaled_1e6_And_RoundUp 0 1234567 Tasty.@?= 0
        , Tasty.testCase "Divide 0 by any number safely" $
            OnChainHelpers.divide_By_Scaled_1e6_And_RoundDownSafe 0 1234567 Tasty.@?= 0
        , Tasty.testCase "Divide with remainder" $
            OnChainHelpers.divide_By_Scaled_1e6_And_RoundDownSafe 10 3000000 Tasty.@?= 3

        -- New tests for multiply_By_Scaled_1e6_And_RoundUp
        , Tasty.testCase "Multiply_By_Scaled_1e6_And_RoundUp positive" $
            OnChainHelpers.multiply_By_Scaled_1e6_And_RoundUp 100 1050000 Tasty.@?= 105
        , Tasty.testCase "Multiply_By_Scaled_1e6_And_RoundUp negative" $
            OnChainHelpers.multiply_By_Scaled_1e6_And_RoundUp (-100) 1050000 Tasty.@?= -105
        , Tasty.testCase "Multiply_By_Scaled_1e6_And_RoundUp positive with remainder" $
            OnChainHelpers.multiply_By_Scaled_1e6_And_RoundUp 100 1049999 Tasty.@?= 105
        , Tasty.testCase "Multiply_By_Scaled_1e6_And_RoundUp negative with remainder" $
            OnChainHelpers.multiply_By_Scaled_1e6_And_RoundUp (-100) 1049999 Tasty.@?= -105

        -- New tests for multiply_By_Scaled_1e6_And_RoundDown
        , Tasty.testCase "Multiply_By_Scaled_1e6_And_RoundDown positive" $
            OnChainHelpers.multiply_By_Scaled_1e6_And_RoundDown 100 1050000 Tasty.@?= 105
        , Tasty.testCase "Multiply_By_Scaled_1e6_And_RoundDown negative" $
            OnChainHelpers.multiply_By_Scaled_1e6_And_RoundDown (-100) 1050000 Tasty.@?= -105
        , Tasty.testCase "Multiply_By_Scaled_1e6_And_RoundDown positive with remainder" $
            OnChainHelpers.multiply_By_Scaled_1e6_And_RoundDown 100 1049999 Tasty.@?= 104
        , Tasty.testCase "Multiply_By_Scaled_1e6_And_RoundDown negative with remainder" $
            OnChainHelpers.multiply_By_Scaled_1e6_And_RoundDown (-100) 1049999 Tasty.@?= -104

        -- New tests for divide_By_Scaled_1e6_And_RoundDownSafe
        , Tasty.testCase "Divide_By_Scaled_1e6_And_RoundDownSafe positive / positive" $
            OnChainHelpers.divide_By_Scaled_1e6_And_RoundDownSafe 105 1000000 Tasty.@?= 105
        , Tasty.testCase "Divide_By_Scaled_1e6_And_RoundDownSafe positive / positive with rounding" $
            OnChainHelpers.divide_By_Scaled_1e6_And_RoundDownSafe 105 1050000 Tasty.@?= 100
        , Tasty.testCase "Divide_By_Scaled_1e6_And_RoundDownSafe negative / positive" $
            OnChainHelpers.divide_By_Scaled_1e6_And_RoundDownSafe (-105) 1000000 Tasty.@?= -105
        , Tasty.testCase "Divide_By_Scaled_1e6_And_RoundDownSafe negative / positive with rounding" $
            OnChainHelpers.divide_By_Scaled_1e6_And_RoundDownSafe (-105) 1050000 Tasty.@?= -100
        , Tasty.testCase "Divide_By_Scaled_1e6_And_RoundDownSafe positive / negative" $
            OnChainHelpers.divide_By_Scaled_1e6_And_RoundDownSafe 105 (-1000000) Tasty.@?= -105
        , Tasty.testCase "Divide_By_Scaled_1e6_And_RoundDownSafe negative / negative" $
            OnChainHelpers.divide_By_Scaled_1e6_And_RoundDownSafe (-105) (-1000000) Tasty.@?= 105
        , Tasty.testCase "Divide_By_Scaled_1e6_And_RoundDownSafe small positive / large positive" $
            OnChainHelpers.divide_By_Scaled_1e6_And_RoundDownSafe 1 2000000 Tasty.@?= 0
        , Tasty.testCase "Divide_By_Scaled_1e6_And_RoundDownSafe small negative / large positive" $
            OnChainHelpers.divide_By_Scaled_1e6_And_RoundDownSafe (-1) 2000000 Tasty.@?= 0

        -- Test for multiplication close to overflow
        , Tasty.testCase "Multiply large number close to overflow" $ do
            let maxInt = fromIntegral (P.maxBound :: Int)
                halfMax = maxInt `div` 2
                result = OnChainHelpers.multiply_By_Scaled_1e6_And_RoundUp halfMax 2000000
            Tasty.assertBool
                (unlines
                [ "Multiplication close to overflow:"
                , "Input: " ++ show halfMax
                , "Scaling factor: 2000000"
                , "Result: " ++ show result
                , "Expected: " ++ show maxInt
                , "Difference: " ++ show (maxInt - result)
                ])
                (result >= maxInt - 1 && result <= maxInt)
        ]
