--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4

module TestTree.ScaleAndRunding where

-- Non-IOG imports
import           Prelude                hiding (length)
import qualified Test.QuickCheck        as QC
import qualified Test.Tasty             as Tasty
import qualified Test.Tasty.QuickCheck  as TastyQC

-- IOG imports
import qualified PlutusTx.Ratio         as TxRatio

-- Project imports
import qualified Data.List              as DataList
import qualified Generic.OnChainHelpers as OnChainHelpers
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------

-- Helper function for detailed logging
logDetails :: String -> [(String, Integer)] -> String
logDetails opName values = DataList.intercalate "\n" $
    ("Operation: " ++ opName) : map (\(name, val) -> name ++ ": " ++ show val) values

--------------------------------------------------------------------------------

scale_And_Runding_Tests :: AppM Tasty.TestTree
scale_And_Runding_Tests =
    return $ Tasty.testGroup "Tests"
      [ TastyQC.testProperty "Multiply-Divide Round Trip (1e6)" prop_multiplyDivideRoundTrip_1e6
      , TastyQC.testProperty "Divide-Multiply Round Trip (1e6)" prop_divideMultiplyRoundTrip_1e6
      , TastyQC.testProperty "Round Up >= Round Down (1e6)" prop_roundUpGeRoundDown_1e6
      , TastyQC.testProperty "TxRatio truncate vs multiply_By_Scaled_1e6_And_RoundDown" prop_txRatioVsMultiplyRoundDown_1e6
      , TastyQC.testProperty "TxRatio truncate vs integer division" prop_txRatioVsIntegerDivision
      -- New properties for 1e2 scaling
      , TastyQC.testProperty "Multiply-Divide Round Trip (1e2)" prop_multiplyDivideRoundTrip_1e2
      , TastyQC.testProperty "Divide-Multiply Round Trip (1e2)" prop_divideMultiplyRoundTrip_1e2
      , TastyQC.testProperty "Round Up >= Round Down (1e2)" prop_roundUpGeRoundDown_1e2
      , TastyQC.testProperty "TxRatio truncate vs multiply_By_Scaled_1e2_And_RoundDown" prop_txRatioVsMultiplyRoundDown_1e2
      -- New properties for BPx1e3 scaling
      , TastyQC.testProperty "Multiply-Divide Round Trip (BPx1e3)" prop_multiplyDivideRoundTrip_BPx1e3
      , TastyQC.testProperty "Divide-Multiply Round Trip (BPx1e3)" prop_divideMultiplyRoundTrip_BPx1e3
      , TastyQC.testProperty "Round Up >= Round Down (BPx1e3)" prop_roundUpGeRoundDown_BPx1e3
      , TastyQC.testProperty "TxRatio truncate vs multiply_By_Scaled_BPx1e3_And_RoundDown" prop_txRatioVsMultiplyRoundDown_BPx1e3
      ]

--------------------------------------------------------------------------------

prop_multiplyDivideRoundTrip_1e6 :: Integer -> Integer -> QC.Property
prop_multiplyDivideRoundTrip_1e6 amount price =
  amount > 0 && price > 0 TastyQC.==>
    let price1e6 = price * 1_000_000
        multiplied = OnChainHelpers.multiply_By_Scaled_1e6_And_RoundUp amount price1e6
        divided = OnChainHelpers.divide_By_Scaled_1e6_And_RoundDownSafe multiplied price1e6
    in QC.counterexample (logDetails "Multiply-Divide Round Trip"
         [ ("amount", amount)
         , ("price", price)
         , ("price1e6", price1e6)
         , ("multiplied", multiplied)
         , ("divided", divided)
         ]) $
       divided == amount

prop_divideMultiplyRoundTrip_1e6 :: Integer -> Integer -> QC.Property
prop_divideMultiplyRoundTrip_1e6 total price =
  total > 0 && price > 0 TastyQC.==>
    let price1e6 = price * 1_000_000
        divided = OnChainHelpers.divide_By_Scaled_1e6_And_RoundDownSafe total price1e6
        multiplied = OnChainHelpers.multiply_By_Scaled_1e6_And_RoundUp divided price1e6
    in QC.counterexample (logDetails "Divide-Multiply Round Trip"
         [ ("total", total)
         , ("price", price)
         , ("price1e6", price1e6)
         , ("divided", divided)
         , ("multiplied", multiplied)
         ]) $
       multiplied <= total

prop_roundUpGeRoundDown_1e6 :: Integer -> Integer -> QC.Property
prop_roundUpGeRoundDown_1e6 amount price =
  amount > 0 && price > 0 TastyQC.==>
    let price1e6 = price * 1_000_000
        roundUp = OnChainHelpers.multiply_By_Scaled_1e6_And_RoundUp amount price1e6
        roundDown = OnChainHelpers.multiply_By_Scaled_1e6_And_RoundDown amount price1e6
    in QC.counterexample (logDetails "Round Up >= Round Down"
         [ ("amount", amount)
         , ("price", price)
         , ("price1e6", price1e6)
         , ("roundUp", roundUp)
         , ("roundDown", roundDown)
         ]) $
       roundUp >= roundDown

prop_safeDivisionLeRegularDivision_1e6 :: Integer -> Integer -> QC.Property
prop_safeDivisionLeRegularDivision_1e6 amount price =
  amount > 0 && price > 0 TastyQC.==>
    let price1e6 = price * 1_000_000
        safeDivision = OnChainHelpers.divide_By_Scaled_1e6_And_RoundDownSafe amount price1e6
        regularDivision = (amount * 1_000_000) `div` price1e6
    in QC.counterexample (logDetails "Safe Division <= Regular Division"
         [ ("amount", amount)
         , ("price", price)
         , ("price1e6", price1e6)
         , ("safeDivision", safeDivision)
         , ("regularDivision", regularDivision)
         ]) $
       safeDivision <= regularDivision

-- New property test: Compare TxRatio.truncate with multiply_By_Scaled_1e6_And_RoundDown
prop_txRatioVsMultiplyRoundDown_1e6 :: Integer -> Integer -> QC.Property
prop_txRatioVsMultiplyRoundDown_1e6  deposit price1e6 =
    let txRatioResult = TxRatio.truncate (TxRatio.unsafeRatio (price1e6 * deposit) 1_000_000)
        scaleResult = OnChainHelpers.multiply_By_Scaled_1e6_And_RoundDown deposit price1e6
    in QC.counterexample (logDetails "TxRatio truncate vs multiply_By_Scaled_1e6_And_RoundDown"
         [
          ("deposit", deposit)
         , ("price1e6", price1e6)
         , ("txRatioResult", txRatioResult)
         , ("scaleResult", scaleResult)
         ]) $
       txRatioResult == scaleResult

-- New property test: Compare TxRatio.truncate with integer division
prop_txRatioVsIntegerDivision :: Integer -> Integer -> QC.Property
prop_txRatioVsIntegerDivision numerator denominator =
  denominator /= 0 TastyQC.==>
    let txRatioResult = TxRatio.truncate (TxRatio.unsafeRatio numerator denominator)
        intDivResult =  OnChainHelpers.divide_By_Scaled_And_RoundDownSafe numerator denominator 1
    in QC.counterexample (logDetails "TxRatio truncate vs integer division"
         [ ("numerator", numerator)
         , ("denominator", denominator)
         , ("txRatioResult", txRatioResult)
         , ("intDivResult", intDivResult)
         ]) $
       txRatioResult == intDivResult

----------------------------------------------------------------
-- New properties for 1e2 scaling
prop_multiplyDivideRoundTrip_1e2 :: Integer -> Integer -> QC.Property
prop_multiplyDivideRoundTrip_1e2 amount price =
  amount > 0 && price > 0  TastyQC.==>
    let price1e2 = price * 100
        multiplied = OnChainHelpers.multiply_By_Scaled_1e2_And_RoundUp amount price1e2
        divided = OnChainHelpers.divide_By_Scaled_1e2_And_RoundDownSafe multiplied price1e2
    in QC.counterexample (logDetails "Multiply-Divide Round Trip (1e2)"
         [ ("amount", amount)
         , ("price", price)
         , ("price1e2", price1e2)
         , ("multiplied", multiplied)
         , ("divided", divided)
         ]) $
       divided == amount

prop_divideMultiplyRoundTrip_1e2 :: Integer -> Integer -> QC.Property
prop_divideMultiplyRoundTrip_1e2 total price =
  total > 0 && price > 0  TastyQC.==>
    let price1e2 = price * 100
        divided = OnChainHelpers.divide_By_Scaled_1e2_And_RoundDownSafe total price1e2
        multiplied = OnChainHelpers.multiply_By_Scaled_1e2_And_RoundUp divided price1e2
    in QC.counterexample (logDetails "Divide-Multiply Round Trip (1e2)"
         [ ("total", total)
         , ("price", price)
         , ("price1e2", price1e2)
         , ("divided", divided)
         , ("multiplied", multiplied)
         ]) $
       multiplied <= total

prop_roundUpGeRoundDown_1e2 :: Integer -> Integer -> QC.Property
prop_roundUpGeRoundDown_1e2 amount price =
  amount > 0 && price > 0 TastyQC.==>
    let price1e2 = price * 100
        roundUp = OnChainHelpers.multiply_By_Scaled_1e2_And_RoundUp amount price1e2
        roundDown = OnChainHelpers.multiply_By_Scaled_1e2_And_RoundDown amount price1e2
    in QC.counterexample (logDetails "Round Up >= Round Down (1e2)"
         [ ("amount", amount)
         , ("price", price)
         , ("price1e2", price1e2)
         , ("roundUp", roundUp)
         , ("roundDown", roundDown)
         ]) $
       roundUp >= roundDown

prop_txRatioVsMultiplyRoundDown_1e2 :: Integer -> Integer -> QC.Property
prop_txRatioVsMultiplyRoundDown_1e2 deposit price1e2 =
    let txRatioResult = TxRatio.truncate (TxRatio.unsafeRatio (price1e2 * deposit) 100)
        scaleResult = OnChainHelpers.multiply_By_Scaled_1e2_And_RoundDown deposit price1e2
    in QC.counterexample (logDetails "TxRatio truncate vs multiply_By_Scaled_1e2_And_RoundDown"
         [ ("deposit", deposit)
         , ("price1e2", price1e2)
         , ("txRatioResult", txRatioResult)
         , ("scaleResult", scaleResult)
         ]) $
       txRatioResult == scaleResult

----------------------------------------------------------------

-- New properties for BPx1e3 scaling
prop_multiplyDivideRoundTrip_BPx1e3 :: Integer -> Integer -> QC.Property
prop_multiplyDivideRoundTrip_BPx1e3 amount price =
  amount > 0 && price > 0  TastyQC.==>
    let priceBPx1e3 = price * 10000000
        multiplied = OnChainHelpers.multiply_By_Scaled_BPx1e3_And_RoundUp amount priceBPx1e3
        divided = OnChainHelpers.divide_By_Scaled_BPx1e3_And_RoundDownSafe multiplied priceBPx1e3
    in QC.counterexample (logDetails "Multiply-Divide Round Trip (BPx1e3)"
         [ ("amount", amount)
         , ("price", price)
         , ("priceBPx1e3", priceBPx1e3)
         , ("multiplied", multiplied)
         , ("divided", divided)
         ]) $
       divided == amount

prop_divideMultiplyRoundTrip_BPx1e3 :: Integer -> Integer -> QC.Property
prop_divideMultiplyRoundTrip_BPx1e3 total price =
  total > 0 && price > 0 TastyQC.==>
    let priceBPx1e3 = price * 10000000
        divided = OnChainHelpers.divide_By_Scaled_BPx1e3_And_RoundDownSafe total priceBPx1e3
        multiplied = OnChainHelpers.multiply_By_Scaled_BPx1e3_And_RoundUp divided priceBPx1e3
    in QC.counterexample (logDetails "Divide-Multiply Round Trip (BPx1e3)"
         [ ("total", total)
         , ("price", price)
         , ("priceBPx1e3", priceBPx1e3)
         , ("divided", divided)
         , ("multiplied", multiplied)
         ]) $
       multiplied <= total

prop_roundUpGeRoundDown_BPx1e3 :: Integer -> Integer -> QC.Property
prop_roundUpGeRoundDown_BPx1e3 amount price =
  amount > 0 && price > 0 TastyQC.==>
    let priceBPx1e3 = price * 10000000
        roundUp = OnChainHelpers.multiply_By_Scaled_BPx1e3_And_RoundUp amount priceBPx1e3
        roundDown = OnChainHelpers.multiply_By_Scaled_BPx1e3_And_RoundDown amount priceBPx1e3
    in QC.counterexample (logDetails "Round Up >= Round Down (BPx1e3)"
         [ ("amount", amount)
         , ("price", price)
         , ("priceBPx1e3", priceBPx1e3)
         , ("roundUp", roundUp)
         , ("roundDown", roundDown)
         ]) $
       roundUp >= roundDown

prop_txRatioVsMultiplyRoundDown_BPx1e3 :: Integer -> Integer -> QC.Property
prop_txRatioVsMultiplyRoundDown_BPx1e3 deposit priceBPx1e3 =
    let txRatioResult = TxRatio.truncate (TxRatio.unsafeRatio (priceBPx1e3 * deposit) 10000000)
        scaleResult = OnChainHelpers.multiply_By_Scaled_BPx1e3_And_RoundDown deposit priceBPx1e3
    in QC.counterexample (logDetails "TxRatio truncate vs multiply_By_Scaled_BPx1e3_And_RoundDown"
         [ ("deposit", deposit)
         , ("priceBPx1e3", priceBPx1e3)
         , ("txRatioResult", txRatioResult)
         , ("scaleResult", scaleResult)
         ]) $
       txRatioResult == scaleResult

----------------------------------------------------------------
