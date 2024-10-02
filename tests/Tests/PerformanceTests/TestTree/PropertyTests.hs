
--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3
{-# LANGUAGE TypeApplications #-}

module TestTree.PropertyTests where

------------------------------------------------------------------------------------------
-- External Imports
------------------------------------------------------------------------------------------

import qualified Ledger.Ada                      as LedgerAda
import qualified Ledger.Value                    as LedgerValue
import qualified Plutus.V2.Ledger.Api            as LedgerApiV2
import           PlutusTx.Prelude                ()
import           Prelude
import qualified Test.QuickCheck                 as QC
import qualified Test.Tasty                      as Tasty
import qualified Test.Tasty.QuickCheck           as TastyQC

------------------------------------------------------------------------------------------
-- Internal Imports
------------------------------------------------------------------------------------------
import qualified Control.Monad                   as ControlMonad
import qualified Generic.OffChainEvalTesting     as OffChainEvalTesting
import qualified Generic.OffChainHelpers         as OffChainHelpers
import qualified Generic.OnChainHelpers          as OnChainHelpers
import qualified Protocol.Fund.Helpers           as Helpers
import qualified Protocol.Types                  as InvestUnitT
import           TestUtils.Contracts.InitialData
import           TestUtils.TypesMAYZ

------------------------------------------------------------------------------------------
-- Module
------------------------------------------------------------------------------------------

property_Tests :: TestParams -> Tasty.TestTree
property_Tests tp =
    Tasty.testGroup
        "Property-Based Tests"
        [ propertyTestsResources tp
        ]

------------------------------------------------------------------------------------------

propertyTestsResources :: TestParams -> Tasty.TestTree
propertyTestsResources tp = 
    Tasty.testGroup
        "Testing resources usage"
        [ TastyQC.testProperty "PowRational should use less than 3.5Mb" (propPowRational_less_than False 3_500_000)
        , TastyQC.testProperty "CalculateDepositCommissionsUsingMonths should use less than 6.5Mb" (propCalculateDepositCommissionsUsingMonths_less_than False 6_500_000)
        , TastyQC.testProperty "Deposits should use less than 10Mb" (propDeposit_less_than tp False 10_000_000)
        , TastyQC.testProperty "Deposits should be valid" (propDeposit_isValid tp False)
        , TastyQC.testProperty "PowRational Optimized should use less than 1.4Mb" (propPowRational_less_than True 1_400_000)
        , TastyQC.testProperty "CalculateDepositCommissionsUsingMonths Optimized should use less than 2.5Mb" (propCalculateDepositCommissionsUsingMonths_less_than True 2_500_000)
        , TastyQC.testProperty "Deposits Optimized with 10 max tokens should use less than 6.2Mb" (propDeposit_less_than tp True 6_250_000)
        , TastyQC.testProperty "Deposits Optimized should be valid" (propDeposit_isValid tp True)
        ]

---------------------------------------------------------------

data TestParamsCalculateDepositCommissionsUsingMonths
    = TestParamsCalculateDepositCommissionsUsingMonths Integer LedgerApiV2.POSIXTime LedgerApiV2.POSIXTime Integer
    deriving (Eq, Show)

instance QC.Arbitrary TestParamsCalculateDepositCommissionsUsingMonths where
    arbitrary = do
        commissionPerYearInBPx1e3 <- QC.choose (1, 10000000)
        deadline' <- QC.choose (1691965154000, 1755123554000)
        date' <- QC.choose (1691965154000, 1755123554000)
        deposit <- QC.choose (1000000, 10000000000)
        return (TestParamsCalculateDepositCommissionsUsingMonths commissionPerYearInBPx1e3 (LedgerApiV2.POSIXTime deadline') (LedgerApiV2.POSIXTime date') deposit)

propCalculateDepositCommissionsUsingMonths_less_than :: Bool -> Integer -> TestParamsCalculateDepositCommissionsUsingMonths -> QC.Property
propCalculateDepositCommissionsUsingMonths_less_than useOptimized memMax (TestParamsCalculateDepositCommissionsUsingMonths commissionPerYearInBPx1e3 deadline date deposit) =
    (deadline > date) QC.==>
        runCheckCalculateDepositCommissionsUsingMonths_less_than useOptimized memMax commissionPerYearInBPx1e3 deadline date deposit

runCheckCalculateDepositCommissionsUsingMonths_less_than :: Bool -> Integer -> Integer -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> QC.Property
runCheckCalculateDepositCommissionsUsingMonths_less_than useOptimized memMax commissionPerYearInBPx1e3 deadline date deposit =
    let
        evalCostResult =
            if useOptimized
                then OffChainEvalTesting.evaluateCompileCodeWithCekGetCost (OffChainEvalTesting.calculateDepositCommissionsUsingMonthsBuiltinDataCodeOptimized commissionPerYearInBPx1e3 deadline date deposit)
                else OffChainEvalTesting.evaluateCompileCodeWithCekGetCost (OffChainEvalTesting.calculateDepositCommissionsUsingMonthsBuiltinDataCode commissionPerYearInBPx1e3 deadline date deposit)
        (LedgerApiV2.ExBudget _ (LedgerApiV2.ExMemory mem), _) = evalCostResult
        memInt = read @Integer (show mem)
    in
        TastyQC.counterexample (show evalCostResult) (memInt < memMax)

---------------------------------------------------------------

data TestParamsPowRational
    = TestParamsPowRational Integer Integer Integer
    deriving (Eq, Show)

instance QC.Arbitrary TestParamsPowRational where
    arbitrary = do
        num <- QC.choose (0, 120_000_000)
        den <- QC.choose (0, 120_000_000)
        n <- QC.choose (0, 25)
        return (TestParamsPowRational num den n)

propPowRational_less_than :: Bool -> Integer -> TestParamsPowRational -> QC.Property
propPowRational_less_than useOptimized memMax (TestParamsPowRational num den n) =
    True QC.==>
        runCheckPowRational_less_than useOptimized memMax num den n

runCheckPowRational_less_than :: Bool -> Integer -> Integer -> Integer -> Integer -> QC.Property
runCheckPowRational_less_than useOptimized memMax num den n =
    let
        evalCostResult =
            if useOptimized
                then OffChainEvalTesting.evaluateCompileCodeWithCekGetCost (OffChainEvalTesting.powRationalWrapperBuiltinDataCodeOptimized num den n)
                else OffChainEvalTesting.evaluateCompileCodeWithCekGetCost (OffChainEvalTesting.powRationalWrapperBuiltinDataCode num den n)

        (LedgerApiV2.ExBudget _ (LedgerApiV2.ExMemory mem), _) = evalCostResult
        memInt = read @Integer (show mem)
    in
        TastyQC.counterexample (show evalCostResult) (memInt < memMax)

------------------------------------------------------------------------------------------

data TestParamsDeposit
    = TestParamsDeposit [InvestUnitT.InvestUnitToken] Integer LedgerApiV2.POSIXTime LedgerApiV2.POSIXTime Integer
    deriving (Eq, Show)

instance QC.Arbitrary TestParamsDeposit where
    arbitrary = do
        tokensQty <- QC.choose (5, 10)
        tokens <- ControlMonad.replicateM tokensQty generateRandomToken
        commissionPerYearInBPx1e3 <- QC.choose (1, 10000000)
        deadline' <- QC.choose (1691965154000, 1755123554000)
        date' <- QC.choose (1691965154000, 1755123554000)
        deposit <- QC.choose (1000000, 10000000000)
        return (TestParamsDeposit tokens commissionPerYearInBPx1e3 (LedgerApiV2.POSIXTime deadline') (LedgerApiV2.POSIXTime date') deposit)

generateRandomToken :: QC.Gen InvestUnitT.InvestUnitToken
generateRandomToken = do
    randomHex <- ControlMonad.replicateM 64 (QC.elements ['0' .. 'f'])
    randomTokenName <- ControlMonad.replicateM 5 (QC.elements ['a' .. 'z'])
    return (LedgerValue.CurrencySymbol $ OffChainHelpers.stringToBuiltinByteString randomHex, LedgerValue.TokenName $ OffChainHelpers.stringToBuiltinByteString randomTokenName, 1)

propDeposit_less_than :: TestParams -> Bool -> Integer -> TestParamsDeposit -> QC.Property
propDeposit_less_than tp useOptimized memMax (TestParamsDeposit tokens commissionPerYearInBPx1e3 deadline date deposit) =
    (deadline > date) QC.==>
        runCheckDeposit_less_than tp useOptimized memMax tokens commissionPerYearInBPx1e3 deadline date deposit

runCheckDeposit_less_than :: TestParams -> Bool -> Integer -> [InvestUnitT.InvestUnitToken] -> Integer -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> QC.Property
runCheckDeposit_less_than tp useOptimized memMax tokens commissionPerYearInBPx1e3 deadline date deposit =
    let
        (fundFT_AC, valueOf_FundHoldingDatum_In, valueOf_FundHoldingDatum_Out, investUnit) = generateDepositParams tp tokens commissionPerYearInBPx1e3 deadline date deposit
        evalCostResult =
            if useOptimized
                then OffChainEvalTesting.evaluateCompileCodeWithCekGetCost (OffChainEvalTesting.testDepositBuiltinDataCodeOptimized commissionPerYearInBPx1e3 deadline date deposit fundFT_AC valueOf_FundHoldingDatum_In valueOf_FundHoldingDatum_Out investUnit)
                else OffChainEvalTesting.evaluateCompileCodeWithCekGetCost (OffChainEvalTesting.testDepositBuiltinDataCode commissionPerYearInBPx1e3 deadline date deposit fundFT_AC valueOf_FundHoldingDatum_In valueOf_FundHoldingDatum_Out investUnit)
        (LedgerApiV2.ExBudget _ (LedgerApiV2.ExMemory mem), _) = evalCostResult
        memInt = read @Integer (show mem)
    in
        TastyQC.counterexample (show evalCostResult) (memInt < memMax)

propDeposit_isValid :: TestParams -> Bool -> TestParamsDeposit -> QC.Property
propDeposit_isValid tp useOptimized (TestParamsDeposit tokens commissionPerYearInBPx1e3 deadline date deposit) =
    (deadline > date) QC.==>
        runCheckDeposit_isValid tp useOptimized tokens commissionPerYearInBPx1e3 deadline date deposit

runCheckDeposit_isValid :: TestParams -> Bool -> [InvestUnitT.InvestUnitToken] -> Integer -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> QC.Property
runCheckDeposit_isValid tp _ tokens commissionPerYearInBPx1e3 deadline date deposit =
    let
        (fundFT_AC, valueOf_FundHoldingDatum_In, valueOf_FundHoldingDatum_Out, investUnit) = generateDepositParams tp tokens commissionPerYearInBPx1e3 deadline date deposit

        !monthsRemainingPlusOne = Helpers.getRemainingMonths deadline (tpBeginAt tp) + 1
        -- defino den = 1e3 * 100 * 100 * 12 = 1000 * 100 * 100 * 12 = 120 000 000
        !den = 120_000_000
        !commissionsTable_Numerator1e6 = [OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator $ OnChainHelpers.powRational (den - commissionPerYearInBPx1e3) den month | month <- [0 .. monthsRemainingPlusOne]]

        isValidDeposit = OffChainEvalTesting.testDeposit commissionsTable_Numerator1e6 deadline date deposit fundFT_AC valueOf_FundHoldingDatum_In valueOf_FundHoldingDatum_Out investUnit
    in
        TastyQC.counterexample (show isValidDeposit) isValidDeposit

generateDepositParams :: TestParams -> [InvestUnitT.InvestUnitToken] -> Integer -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> (LedgerValue.AssetClass, LedgerValue.Value, LedgerValue.Value, InvestUnitT.InvestUnit)
generateDepositParams tp tokens commissionPerYearInBPx1e3 deadline date deposit =
    let
        !monthsRemainingPlusOne = Helpers.getRemainingMonths deadline (tpBeginAt tp) + 1
        -- defino den = 1e3 * 100 * 100 * 12 = 1000 * 100 * 100 * 12 = 120 000 000
        !den = 120_000_000
        !commissionsTable_Numerator1e6 = [OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator $ OnChainHelpers.powRational (den - commissionPerYearInBPx1e3) den month | month <- [0 .. monthsRemainingPlusOne]]

        !(_, commissionsFT, _) = Helpers.calculateDepositCommissionsUsingMonths commissionsTable_Numerator1e6 deadline date deposit

        fundFT_AC :: LedgerValue.AssetClass
        fundFT_AC = LedgerValue.AssetClass (tpFundPolicy_CS tp, tpFundFT_TN tp)

        valueOf_FundHoldingDatum_In :: LedgerValue.Value
        valueOf_FundHoldingDatum_In =
            LedgerAda.lovelaceValueOf minAdaFundDatum
                <> LedgerApiV2.singleton "74854c7cd622e151aeef59b7d97fe0d60e8e69a10adbe13c19e918aa" "FundHoldingID0" 1

        -- valueFor_FundHoldingDatum_Out :: LedgerValue.Value
        -- valueFor_FundHoldingDatum_Out = LedgerAda.lovelaceValueOf 12241330
        --             <> LedgerApiV2.singleton "74854c7cd622e151aeef59b7d97fe0d60e8e69a10adbe13c19e918aa" "FundHoldingID0" 1
        --             <>  LedgerApiV2.singleton "6aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759" "token" deposit
        --             <>  LedgerApiV2.singleton tpFundPolicy_CS tp  T.fundFT_TN commissionsFT

        -- Invest unit tokens are multiplied by 100 to add precision
        adjustedTokens = [(cs, tn, amt * 100) | (cs, tn, amt) <- tokens]

        investUnit :: InvestUnitT.InvestUnit
        investUnit = InvestUnitT.InvestUnit {InvestUnitT.iuValues = adjustedTokens}

        !valueOf_TokensForDeposit =
            foldl
                (<>)
                (LedgerAda.lovelaceValueOf 0)
                [ LedgerValue.assetClassValue
                    ( LedgerValue.AssetClass
                        (cs, tn)
                    )
                    (amt * deposit)
                | (cs, tn, amt) <- tokens
                ]

        !valueFor_FT_Commissions = LedgerValue.assetClassValue fundFT_AC commissionsFT

        !valueFor_FundHoldingDatum_Out = valueOf_FundHoldingDatum_In <> valueOf_TokensForDeposit <> valueFor_FT_Commissions
    in
        -- token1 :: InvestUnitT.InvestUnitToken
        -- token1 = ( "6aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759","token", 1) :: InvestUnitT.InvestUnitToken

        (fundFT_AC, valueOf_FundHoldingDatum_In, valueFor_FundHoldingDatum_Out, investUnit)

------------------------------------------------------------------------------------------
