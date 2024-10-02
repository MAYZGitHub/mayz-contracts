--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4

{- |
Module      : TestTree.Commissions
Description : Unit tests for calculating commissions.

This module provides unit tests for the helper functions related to the
calculation of commissions.
-}
module TestTree.Commissions where
--------------------------------------------------------------------------------4

-- Non-IOG imports

import           Prelude                         hiding (fromInteger, (*))
import qualified Test.Tasty                      as Tasty
import qualified Test.Tasty.HUnit                as Tasty

-- IOG imports
import qualified Plutus.V2.Ledger.Api            as LedgerApiV2
import           PlutusTx.Prelude                (fromInteger, (*))
import qualified PlutusTx.Ratio                  as TxRatio

-- Project imports
import qualified Generic.OnChainHelpers          as OnChainHelpers
import qualified Protocol.Fund.Helpers           as FundHelpers
import qualified Protocol.OnChainHelpers         as OnChainHelpers
import qualified Protocol.Types                  as T
import           TestUtils.Contracts.InitialData
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------

den :: Integer
-- defino den = 1e3 * 100 * 100 * 12 = 1000 * 100 * 100 * 12 = 120 000 000
den = 120_000_000

msPerMonth :: Integer
msPerMonth = 2_592_000_000 -- 1000 * 60 * 60 * 24 * 30

calculateFTForUserDeposit :: Integer -> Integer -> Integer -> Integer
calculateFTForUserDeposit commissions amount months =
    TxRatio.truncate
        ( fromInteger amount
            * OnChainHelpers.powRational
                (den - commissions)
                den
                (months + 1)
        )

calculateFTForUserWithdraw :: Integer -> Integer -> Integer -> Integer
calculateFTForUserWithdraw commissions amount months =
    TxRatio.truncate
        ( fromInteger amount
            * OnChainHelpers.powRational
                (den - commissions)
                den
                months
        )

--------------------------------------------------------------------------------

comission_Tests :: TestParams -> Tasty.TestTree
comission_Tests _tp = do
    Tasty.testGroup
        "Tests"
        [ Tasty.testCase
            "SetAndLoosePrecisionGetOnlyNum"
            test_setAndLoosePrecisionGetOnlyNum
        , Tasty.testCase
            "SetAndLoosePrecision1e6GetOnlyNumerator"
            test_setAndLoosePrecision1e6GetOnlyNumerator
        , Tasty.testCase "powInteger" test_powInteger
        , Tasty.testCase "powRational" test_powRational
        , Tasty.testGroup
            "Calculate Deposit Commissions"
            [ Tasty.testCase
                "Integral remaining months"
                test_deposit_integralRemainingMonths
            , Tasty.testCase
                "Fractional remaining months"
                test_deposit_fractionalRemainingMonths
            , Tasty.testCase
                "Less than one remaining month"
                test_deposit_zeroRemainingMonths
            ]
        , Tasty.testGroup
            "Calculate Withdraw Commissions"
            [ Tasty.testCase
                "Integral remaining months"
                test_withdraw_integralRemainingMonths
            , Tasty.testCase
                "Fractional remaining months"
                test_withdraw_fractionalRemainingMonths
            , Tasty.testCase
                "Less than one remaining month"
                test_withdraw_zeroRemainingMonths
            ]
        ]

--------------------------------------------------------------------------------

test_setAndLoosePrecisionGetOnlyNum :: Tasty.Assertion
test_setAndLoosePrecisionGetOnlyNum = do
    OnChainHelpers.setAndLoosePrecisionGetOnlyNum (TxRatio.unsafeRatio 2 10) 1 Tasty.@?= 2
    OnChainHelpers.setAndLoosePrecisionGetOnlyNum (TxRatio.unsafeRatio 3 20) 2 Tasty.@?= 15
    OnChainHelpers.setAndLoosePrecisionGetOnlyNum (TxRatio.unsafeRatio 3 40) 2 Tasty.@?= 7

test_setAndLoosePrecision1e6GetOnlyNumerator :: Tasty.Assertion
test_setAndLoosePrecision1e6GetOnlyNumerator = do
    let r1 = TxRatio.unsafeRatio 2 600_000
        r2 = TxRatio.unsafeRatio 2 700_000
        r3 = TxRatio.unsafeRatio 3 500_000
    OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator r1 Tasty.@?= 3
    OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator r2 Tasty.@?= 2
    OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator r3 Tasty.@?= 6

test_powInteger :: Tasty.Assertion
test_powInteger = do
    OnChainHelpers.powInteger 0 2 Tasty.@?= 0
    OnChainHelpers.powInteger 2 0 Tasty.@?= 1
    OnChainHelpers.powInteger 2 4 Tasty.@?= 16
    OnChainHelpers.powInteger 5 3 Tasty.@?= 125

-- powInteger 1 (-1) Tasty.@?= -1 -- loops forever

test_powRational :: Tasty.Assertion
test_powRational = do
    OnChainHelpers.powRational 2 2 1 Tasty.@?= TxRatio.unsafeRatio 2 2
    OnChainHelpers.powRational 2 3 2 Tasty.@?= TxRatio.unsafeRatio 4 9
    OnChainHelpers.powRational 2 3 3 Tasty.@?= TxRatio.unsafeRatio 8 27
    OnChainHelpers.powRational 2 3 (-3) Tasty.@?= TxRatio.unsafeRatio 27 8

--------------------------------------------------------------------------------
-- Unit Tests for Depositing
--------------------------------------------------------------------------------

test_deposit_integralRemainingMonths :: Tasty.Assertion
test_deposit_integralRemainingMonths = do
    let commissionPerYearInBPx1e3 = 120_000
        monthsCurrentReal = 2
        date = LedgerApiV2.POSIXTime $ msPerMonth * monthsCurrentReal
        monthsTotalReal = 5
        deadline = LedgerApiV2.POSIXTime $ msPerMonth * monthsTotalReal
        monthsRemainingReal = monthsTotalReal - monthsCurrentReal

        deposit = 1_000_000

        !monthsTotal = FundHelpers.getRemainingMonths deadline 0
        !commissionsTable_Numerator1e6 = [OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator $ OnChainHelpers.powRational (den - commissionPerYearInBPx1e3) den month | month <- [0 .. (monthsTotal + 1)]]

        userFT = calculateFTForUserDeposit commissionPerYearInBPx1e3 deposit monthsRemainingReal

        commissionsFT = deposit - userFT
        commissionsRatePerMonth = TxRatio.unsafeRatio commissionsFT monthsRemainingReal
        commissions_FT_Rate1e6_PerMonth =
            OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator commissionsRatePerMonth

        (userFT', commissionsFT', commissions_FT_Rate1e6_PerMonth') =
            FundHelpers.calculateDepositCommissionsUsingMonths
                commissionsTable_Numerator1e6
                deadline
                date
                deposit

        !monthsRemainingCalculated = FundHelpers.getRemainingMonths deadline date

    -- DebugTrace.trace ("commissionsTable_Numerator1e6: " ++ show (commissionsTable_Numerator1e6)) $
    --      DebugTrace.trace ("userFT: " ++ show (userFT)) $
    --         DebugTrace.trace ("monthsRemainingReal: " ++ show ((monthsTotalReal - monthsCurrentReal))) $
    --             DebugTrace.trace ("monthsRemainingCalc: " ++ show (monthsRemaining)) $
    --             DebugTrace.trace ("deadline: " ++ show (deadline)) $
    --             DebugTrace.trace ("date: " ++ show (date)) $
    --             DebugTrace.trace ("ms: " ++ show (LedgerApiV2.getPOSIXTime $ deadline - date)) $
    --             DebugTrace.trace ("div: " ++ show (LedgerApiV2.getPOSIXTime (deadline - date) `divide` msPerMonth)) $

    (monthsRemainingReal, userFT, commissionsFT, commissions_FT_Rate1e6_PerMonth)
        Tasty.@?= (monthsRemainingCalculated, userFT', commissionsFT', commissions_FT_Rate1e6_PerMonth')

test_deposit_fractionalRemainingMonths :: Tasty.Assertion
test_deposit_fractionalRemainingMonths = do
    let commissionPerYearInBPx1e3 = 300_000
        monthsCurrentReal = 2
        date = LedgerApiV2.POSIXTime $ msPerMonth * monthsCurrentReal
        monthsTotalReal = 5
        deadline = LedgerApiV2.POSIXTime $ msPerMonth * monthsTotalReal + 1_000_000
        monthsRemainingReal = monthsTotalReal - monthsCurrentReal

        deposit = 222

        !monthsTotal = FundHelpers.getRemainingMonths deadline 0
        !commissionsTable_Numerator1e6 = [OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator $ OnChainHelpers.powRational (den - commissionPerYearInBPx1e3) den month | month <- [0 .. (monthsTotal + 1)]]

        userFT = calculateFTForUserDeposit commissionPerYearInBPx1e3 deposit monthsRemainingReal

        commissionsFT = deposit - userFT
        commissionsRatePerMonth = TxRatio.unsafeRatio commissionsFT monthsRemainingReal
        commissions_FT_Rate1e6_PerMonth =
            OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator commissionsRatePerMonth

        (userFT', commissionsFT', commissions_FT_Rate1e6_PerMonth') =
            FundHelpers.calculateDepositCommissionsUsingMonths
                commissionsTable_Numerator1e6
                deadline
                date
                deposit

        !monthsRemainingCalculated = FundHelpers.getRemainingMonths deadline date

    (monthsRemainingReal, userFT, commissionsFT, commissions_FT_Rate1e6_PerMonth)
        Tasty.@?= (monthsRemainingCalculated, userFT', commissionsFT', commissions_FT_Rate1e6_PerMonth')

test_deposit_zeroRemainingMonths :: Tasty.Assertion
test_deposit_zeroRemainingMonths = do
    let commissionPerYearInBPx1e3 = 1_000_000
        monthsCurrentReal = 5
        date = LedgerApiV2.POSIXTime $ msPerMonth * monthsCurrentReal
        monthsTotalReal = 5
        deadline = LedgerApiV2.POSIXTime $ msPerMonth * monthsTotalReal + 1_000_000
        monthsRemainingReal = monthsTotalReal - monthsCurrentReal

        deposit = 300

        !monthsTotal = FundHelpers.getRemainingMonths deadline 0
        !commissionsTable_Numerator1e6 = [OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator $ OnChainHelpers.powRational (den - commissionPerYearInBPx1e3) den month | month <- [0 .. (monthsTotal + 1)]]

        userFT = calculateFTForUserDeposit commissionPerYearInBPx1e3 deposit monthsRemainingReal
        commissionsFT = deposit - userFT
        commissions_FT_Rate1e6_PerMonth = 0

        (userFT', commissionsFT', commissions_FT_Rate1e6_PerMonth') =
            FundHelpers.calculateDepositCommissionsUsingMonths
                commissionsTable_Numerator1e6
                deadline
                date
                deposit

        !monthsRemainingCalculated = FundHelpers.getRemainingMonths deadline date

    (monthsRemainingReal, userFT, commissionsFT, commissions_FT_Rate1e6_PerMonth)
        Tasty.@?= (monthsRemainingCalculated, userFT', commissionsFT', commissions_FT_Rate1e6_PerMonth')

--------------------------------------------------------------------------------
-- Unit Tests for Withdrawing
--------------------------------------------------------------------------------

test_withdraw_integralRemainingMonths :: Tasty.Assertion
test_withdraw_integralRemainingMonths = do
    let commissionPerYearInBPx1e3 = 120_000
        monthsCurrentReal = 2
        date = LedgerApiV2.POSIXTime $ msPerMonth * monthsCurrentReal
        monthsTotalReal = 5
        deadline = LedgerApiV2.POSIXTime $ msPerMonth * monthsTotalReal
        monthsRemainingReal = monthsTotalReal - monthsCurrentReal

        withdraw = 100

        !monthsTotal = FundHelpers.getRemainingMonths deadline 0
        !commissionsTable_Numerator1e6 = [OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator $ OnChainHelpers.powRational (den - commissionPerYearInBPx1e3) den month | month <- [0 .. (monthsTotal + 1)]]

        userFT = calculateFTForUserWithdraw commissionPerYearInBPx1e3 withdraw monthsRemainingReal

        commissionsForUserFTToGetBack = withdraw - userFT
        withdrawPlusCommissionsGetBack = withdraw + commissionsForUserFTToGetBack
        commissionsRatePerMonth = TxRatio.unsafeRatio commissionsForUserFTToGetBack monthsRemainingReal
        commissions_FT_Rate1e6_PerMonth =
            OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator commissionsRatePerMonth

        !investUnit_Granularity = OnChainHelpers.getDecimalsInInvestUnit (T.iuValues investUnit_Initial)

        (commissionsForUserFTToGetBack', commisswithdrawPlusCommissionsGetBackionsFT', commissions_FT_Rate1e6_PerMonth') =
            FundHelpers.calculateWithdrawCommissionsUsingMonths
                commissionsTable_Numerator1e6
                deadline
                date
                withdraw
                investUnit_Granularity

        !monthsRemainingCalculated = FundHelpers.getRemainingMonths deadline date

    (monthsRemainingReal, commissionsForUserFTToGetBack, withdrawPlusCommissionsGetBack, commissions_FT_Rate1e6_PerMonth)
        Tasty.@?= (monthsRemainingCalculated, commissionsForUserFTToGetBack', commisswithdrawPlusCommissionsGetBackionsFT', commissions_FT_Rate1e6_PerMonth')

test_withdraw_fractionalRemainingMonths :: Tasty.Assertion
test_withdraw_fractionalRemainingMonths = do
    let commissionPerYearInBPx1e3 = 300_000
        monthsCurrentReal = 2
        date = LedgerApiV2.POSIXTime $ msPerMonth * monthsCurrentReal
        monthsTotalReal = 5
        deadline = LedgerApiV2.POSIXTime $ msPerMonth * monthsTotalReal + 1_000_000
        monthsRemainingReal = monthsTotalReal - monthsCurrentReal

        withdraw = 222

        !monthsTotal = FundHelpers.getRemainingMonths deadline 0
        !commissionsTable_Numerator1e6 = [OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator $ OnChainHelpers.powRational (den - commissionPerYearInBPx1e3) den month | month <- [0 .. (monthsTotal + 1)]]

        userFT = calculateFTForUserWithdraw commissionPerYearInBPx1e3 withdraw monthsRemainingReal
        commissionsForUserFTToGetBack = withdraw - userFT
        withdrawPlusCommissionsGetBack = withdraw + commissionsForUserFTToGetBack
        commissionsRatePerMonth = TxRatio.unsafeRatio commissionsForUserFTToGetBack monthsRemainingReal
        commissions_FT_Rate1e6_PerMonth =
            OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator commissionsRatePerMonth

        !investUnit_Granularity = OnChainHelpers.getDecimalsInInvestUnit (T.iuValues investUnit_Initial)

        (commissionsForUserFTToGetBack', commisswithdrawPlusCommissionsGetBackionsFT', commissions_FT_Rate1e6_PerMonth') =
            FundHelpers.calculateWithdrawCommissionsUsingMonths
                commissionsTable_Numerator1e6
                deadline
                date
                withdraw
                investUnit_Granularity

        !monthsRemainingCalculated = FundHelpers.getRemainingMonths deadline date

    (monthsRemainingReal, commissionsForUserFTToGetBack, withdrawPlusCommissionsGetBack, commissions_FT_Rate1e6_PerMonth)
        Tasty.@?= (monthsRemainingCalculated, commissionsForUserFTToGetBack', commisswithdrawPlusCommissionsGetBackionsFT', commissions_FT_Rate1e6_PerMonth')

test_withdraw_zeroRemainingMonths :: Tasty.Assertion
test_withdraw_zeroRemainingMonths = do
    let commissionPerYearInBPx1e3 = 1_000_000
        monthsCurrentReal = 5
        date = LedgerApiV2.POSIXTime $ msPerMonth * monthsCurrentReal
        monthsTotalReal = 5
        deadline = LedgerApiV2.POSIXTime $ msPerMonth * monthsTotalReal + 1_000_000
        monthsRemainingReal = monthsTotalReal - monthsCurrentReal

        withdraw = 100

        !monthsTotal = FundHelpers.getRemainingMonths deadline 0
        !commissionsTable_Numerator1e6 = [OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator $ OnChainHelpers.powRational (den - commissionPerYearInBPx1e3) den month | month <- [0 .. (monthsTotal + 1)]]

        userFT = calculateFTForUserWithdraw commissionPerYearInBPx1e3 withdraw monthsRemainingReal
        commissionsForUserFTToGetBack = withdraw - userFT
        withdrawPlusCommissionsGetBack = withdraw + commissionsForUserFTToGetBack
        commissions_FT_Rate1e6_PerMonth = 0

        !investUnit_Granularity = OnChainHelpers.getDecimalsInInvestUnit (T.iuValues investUnit_Initial)

        (commissionsForUserFTToGetBack', commisswithdrawPlusCommissionsGetBackionsFT', commissions_FT_Rate1e6_PerMonth') =
            FundHelpers.calculateWithdrawCommissionsUsingMonths
                commissionsTable_Numerator1e6
                deadline
                date
                withdraw
                investUnit_Granularity

        !monthsRemainingCalculated = FundHelpers.getRemainingMonths deadline date

    (monthsRemainingReal, commissionsForUserFTToGetBack, withdrawPlusCommissionsGetBack, commissions_FT_Rate1e6_PerMonth)
        Tasty.@?= (monthsRemainingCalculated, commissionsForUserFTToGetBack', commisswithdrawPlusCommissionsGetBackionsFT', commissions_FT_Rate1e6_PerMonth')

--------------------------------------------------------------------------------
