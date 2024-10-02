--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4

{- |
Module      : TestTree.Commissions
Description :
-}
module TestTree.Commissions where

-- Non-IOG imports

import           Prelude
import qualified Test.QuickCheck        as QC
import qualified Test.Tasty             as Tasty
import qualified Test.Tasty.QuickCheck  as TastyQC
-- IOG imports
import qualified Plutus.V2.Ledger.Api   as LedgerApiV2

-- Project imports
import qualified Generic.OnChainHelpers as OnChainHelpers
import qualified Protocol.Fund.Helpers  as FundHelpers
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------

msPerMonth :: Integer
msPerMonth = 2_592_000_000 -- 1000 * 60 * 60 * 24 * 30

den :: Integer
-- defino den = 1e3 * 100 * 100 * 12 = 1000 * 100 * 100 * 12 = 120 000 000
den = 120_000_000

--------------------------------------------------------------------------------

comission_Tests :: AppM Tasty.TestTree
comission_Tests = do
    return $ Tasty.testGroup
        "Tests"
        [ TastyQC.testProperty "Withdraw commissions respect Invest Unit granularity" prop_withdrawCommissionsRespectGranularity
        , TastyQC.testProperty "Deposit commissions are always less than or equal to deposit amount" prop_depositCommissionsLessThanDeposit
        , TastyQC.testProperty "Withdraw plus commissions is always greater than or equal to withdraw amount" prop_withdrawPlusCommissionsGreaterThanWithdraw
        , TastyQC.testProperty "Commission rate decreases over time" prop_commissionRateDecreasesOverTime
        , TastyQC.testProperty "Deposit and immediate withdraw results in loss" prop_depositAndImmediateWithdrawResultsInLoss
        , TastyQC.testProperty "Higher commission rate results in lower user FT" prop_higherCommissionRateLowerUserFT
        , TastyQC.testProperty "Longer fund duration results in higher total commissions" prop_longerDurationHigherTotalCommissions
        ]

--------------------------------------------------------------------------------
{-|
Prueba que los cálculos de comisiones en retiros respetan la granularidad del invest unit.

Esta propiedad verifica que:
1. Se manejan correctamente las tres granularidades posibles (1, 10, 100).
2. El monto de retiro inicial es válido según la granularidad.
3. Las comisiones calculadas para devolver al usuario son divisibles por la granularidad.
4. El monto total (retiro + comisiones) es divisible por la granularidad.

Esto asegura que todos los cálculos relacionados con retiros y comisiones
son consistentes con la estructura del invest unit y no generan fracciones
de tokens que no podrían ser representadas en el sistema.
-}

--------------------------------------------------------------------------------

prop_withdrawCommissionsRespectGranularity :: QC.Property
prop_withdrawCommissionsRespectGranularity =
    QC.forAll (QC.elements [1, 10, 100]) $ \granularity ->
    QC.forAll (QC.choose (1, 1000000)) $ \withdrawBase ->
    QC.forAll (QC.choose (1, 60)) $ \monthsTotalReal ->
    QC.forAll (QC.choose (0, monthsTotalReal - 1)) $ \monthsCurrentReal ->
    QC.forAll (QC.choose (10000, 1000000)) $ \commissionPerYearInBPx1e3 ->
        let
            withdraw = withdrawBase * granularity  -- Asegura que el retiro sea divisible por la granularidad

            date = LedgerApiV2.POSIXTime $ msPerMonth * monthsCurrentReal
            deadline = LedgerApiV2.POSIXTime $ msPerMonth * monthsTotalReal

            !monthsTotal = FundHelpers.getRemainingMonths deadline 0
            !commissionsTable_Numerator1e6 = [OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator $ OnChainHelpers.powRational (den - commissionPerYearInBPx1e3) den month | month <- [0 .. (monthsTotal + 1)]]

            (commissionsForUserFTToGetBack, withdrawPlusCommissionsGetBack, _) =
                FundHelpers.calculateWithdrawCommissionsUsingMonths
                    commissionsTable_Numerator1e6
                    deadline
                    date
                    withdraw
                    granularity

            isWithdrawValid = withdraw `mod` granularity == 0
            isCommissionsValid = commissionsForUserFTToGetBack `mod` granularity == 0
            isWithdrawPlusCommissionsValid = withdrawPlusCommissionsGetBack `mod` granularity == 0

        in QC.counterexample
            (unlines
                [ "Granularity: " ++ show granularity
                , "Withdraw: " ++ show withdraw
                , "Commissions to get back: " ++ show commissionsForUserFTToGetBack
                , "Withdraw plus commissions: " ++ show withdrawPlusCommissionsGetBack
                , "Months total: " ++ show monthsTotalReal
                , "Months current: " ++ show monthsCurrentReal
                , "Commission per year (BPx1e3): " ++ show commissionPerYearInBPx1e3
                ])
            (isWithdrawValid && isCommissionsValid && isWithdrawPlusCommissionsValid)

--------------------------------------------------------------------------------

prop_depositCommissionsLessThanDeposit :: QC.Property
prop_depositCommissionsLessThanDeposit =
    QC.forAll (QC.choose (1, 1000000)) $ \deposit ->
    QC.forAll (QC.choose (1, 60)) $ \monthsTotalReal ->
    QC.forAll (QC.choose (0, monthsTotalReal - 1)) $ \monthsCurrentReal ->
    QC.forAll (QC.choose (0, 1000000)) $ \commissionPerYearInBPx1e3 ->
        let
            date = LedgerApiV2.POSIXTime $ msPerMonth * monthsCurrentReal
            deadline = LedgerApiV2.POSIXTime $ msPerMonth * monthsTotalReal

            !monthsTotal = FundHelpers.getRemainingMonths deadline 0
            !commissionsTable_Numerator1e6 = [OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator $ OnChainHelpers.powRational (den - commissionPerYearInBPx1e3) den month | month <- [0 .. (monthsTotal + 1)]]

            (userFT, commissionsFT, _) =
                FundHelpers.calculateDepositCommissionsUsingMonths
                    commissionsTable_Numerator1e6
                    deadline
                    date
                    deposit
        in
        QC.counterexample
            (unlines
                [ "Deposit: " ++ show deposit
                , "Commissions: " ++ show commissionsFT
                , "User FT: " ++ show userFT
                ])
            (commissionsFT <= deposit && userFT <= deposit && commissionsFT + userFT == deposit)

--------------------------------------------------------------------------------

prop_withdrawPlusCommissionsGreaterThanWithdraw :: QC.Property
prop_withdrawPlusCommissionsGreaterThanWithdraw =
    QC.forAll (QC.elements [1, 10, 100]) $ \granularity ->
    QC.forAll (QC.choose (1, 1000000)) $ \withdraw ->
    QC.forAll (QC.choose (1, 60)) $ \monthsTotalReal ->
    QC.forAll (QC.choose (0, monthsTotalReal - 1)) $ \monthsCurrentReal ->
    QC.forAll (QC.choose (0, 1000000)) $ \commissionPerYearInBPx1e3 ->
        let
            date = LedgerApiV2.POSIXTime $ msPerMonth * monthsCurrentReal
            deadline = LedgerApiV2.POSIXTime $ msPerMonth * monthsTotalReal

            !monthsTotal = FundHelpers.getRemainingMonths deadline 0
            !commissionsTable_Numerator1e6 = [OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator $ OnChainHelpers.powRational (den - commissionPerYearInBPx1e3) den month | month <- [0 .. (monthsTotal + 1)]]

            (_, withdrawPlusCommissions, _) =
                FundHelpers.calculateWithdrawCommissionsUsingMonths
                    commissionsTable_Numerator1e6
                    deadline
                    date
                    withdraw
                    granularity
        in
        QC.counterexample
            (unlines
                [ "Withdraw: " ++ show withdraw
                , "Withdraw plus commissions: " ++ show withdrawPlusCommissions
                ])
            (withdrawPlusCommissions >= withdraw)

--------------------------------------------------------------------------------

prop_commissionRateDecreasesOverTime :: QC.Property
prop_commissionRateDecreasesOverTime =
    QC.forAll (QC.choose (1, 1000000)) $ \deposit ->
    QC.forAll (QC.choose (2, 60)) $ \monthsTotalReal ->
    QC.forAll (QC.choose (10000, 1000000)) $ \commissionPerYearInBPx1e3 ->
        let
            deadline = LedgerApiV2.POSIXTime $ msPerMonth * monthsTotalReal

            !monthsTotal = FundHelpers.getRemainingMonths deadline 0
            !commissionsTable_Numerator1e6 = [OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator $ OnChainHelpers.powRational (den - commissionPerYearInBPx1e3) den month | month <- [0 .. (monthsTotal + 1)]]

            calculateCommissionRate month =
                let
                    date = LedgerApiV2.POSIXTime $ msPerMonth * month
                    (_, commissionsFT, _) = FundHelpers.calculateDepositCommissionsUsingMonths
                        commissionsTable_Numerator1e6
                        deadline
                        date
                        deposit
                in
                commissionsFT

            commissionRates = map calculateCommissionRate [0..monthsTotalReal-1]
        in
        QC.counterexample
            (unlines
                [ "Commission rates: " ++ show commissionRates
                ])
            (and $ zipWith (>=) commissionRates (tail commissionRates))

--------------------------------------------------------------------------------

prop_depositAndImmediateWithdrawResultsInLoss :: QC.Property
prop_depositAndImmediateWithdrawResultsInLoss =
    QC.forAll (QC.choose (100, 1000000)) $ \amount ->
    QC.forAll (QC.choose (1, 60)) $ \monthsTotalReal ->
    QC.forAll (QC.choose (10000, 1000000)) $ \commissionPerYearInBPx1e3 ->
        let
            date = LedgerApiV2.POSIXTime 0
            deadline = LedgerApiV2.POSIXTime $ msPerMonth * monthsTotalReal

            !monthsTotal = FundHelpers.getRemainingMonths deadline 0
            !commissionsTable_Numerator1e6 = [OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator $ OnChainHelpers.powRational (den - commissionPerYearInBPx1e3) den month | month <- [0 .. (monthsTotal + 1)]]

            (userFTAfterDeposit, _, _) =
                FundHelpers.calculateDepositCommissionsUsingMonths
                    commissionsTable_Numerator1e6
                    deadline
                    date
                    amount

            (_, withdrawAmount, _) =
                FundHelpers.calculateWithdrawCommissionsUsingMonths
                    commissionsTable_Numerator1e6
                    deadline
                    date
                    userFTAfterDeposit
                    1  -- Using granularity 1 for simplicity
        in
        QC.counterexample
            (unlines
                [ "Initial amount: " ++ show amount
                , "User FT after deposit: " ++ show userFTAfterDeposit
                , "Withdraw amount: " ++ show withdrawAmount
                ])
            (withdrawAmount < amount)

--------------------------------------------------------------------------------

prop_higherCommissionRateLowerUserFT :: QC.Property
prop_higherCommissionRateLowerUserFT =
    QC.forAll (QC.choose (100, 1000000)) $ \deposit ->
    QC.forAll (QC.choose (1, 60)) $ \monthsTotalReal ->
    QC.forAll (QC.choose (10000, 500000)) $ \lowerCommissionRate ->
    QC.forAll (QC.choose (500001, 1000000)) $ \higherCommissionRate ->
        let
            date = LedgerApiV2.POSIXTime 0
            deadline = LedgerApiV2.POSIXTime $ msPerMonth * monthsTotalReal

            !monthsTotal = FundHelpers.getRemainingMonths deadline 0
            !commissionsTableLower = [OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator $ OnChainHelpers.powRational (den - lowerCommissionRate) den month | month <- [0 .. (monthsTotal + 1)]]
            !commissionsTableHigher = [OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator $ OnChainHelpers.powRational (den - higherCommissionRate) den month | month <- [0 .. (monthsTotal + 1)]]

            (userFTLower, _, _) =
                FundHelpers.calculateDepositCommissionsUsingMonths
                    commissionsTableLower
                    deadline
                    date
                    deposit

            (userFTHigher, _, _) =
                FundHelpers.calculateDepositCommissionsUsingMonths
                    commissionsTableHigher
                    deadline
                    date
                    deposit
        in
        QC.counterexample
            (unlines
                [ "Deposit: " ++ show deposit
                , "Lower commission rate: " ++ show lowerCommissionRate
                , "Higher commission rate: " ++ show higherCommissionRate
                , "User FT (lower rate): " ++ show userFTLower
                , "User FT (higher rate): " ++ show userFTHigher
                ])
            (userFTLower > userFTHigher)

--------------------------------------------------------------------------------

prop_longerDurationHigherTotalCommissions :: QC.Property
prop_longerDurationHigherTotalCommissions =
    QC.forAll (QC.choose (100, 1000000)) $ \deposit ->
    QC.forAll (QC.choose (1, 30)) $ \shorterDuration ->
    QC.forAll (QC.choose (31, 60)) $ \longerDuration ->
    QC.forAll (QC.choose (10000, 1000000)) $ \commissionPerYearInBPx1e3 ->
        let
            date = LedgerApiV2.POSIXTime 0
            shorterDeadline = LedgerApiV2.POSIXTime $ msPerMonth * shorterDuration
            longerDeadline = LedgerApiV2.POSIXTime $ msPerMonth * longerDuration

            !shorterMonthsTotal = FundHelpers.getRemainingMonths shorterDeadline 0
            !longerMonthsTotal = FundHelpers.getRemainingMonths longerDeadline 0
            !commissionsTableShorter = [OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator $ OnChainHelpers.powRational (den - commissionPerYearInBPx1e3) den month | month <- [0 .. (shorterMonthsTotal + 1)]]
            !commissionsTableLonger = [OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator $ OnChainHelpers.powRational (den - commissionPerYearInBPx1e3) den month | month <- [0 .. (longerMonthsTotal + 1)]]

            (_, commissionsShorter, _) =
                FundHelpers.calculateDepositCommissionsUsingMonths
                    commissionsTableShorter
                    shorterDeadline
                    date
                    deposit

            (_, commissionsLonger, _) =
                FundHelpers.calculateDepositCommissionsUsingMonths
                    commissionsTableLonger
                    longerDeadline
                    date
                    deposit
        in
        QC.counterexample
            (unlines
                [ "Deposit: " ++ show deposit
                , "Shorter duration: " ++ show shorterDuration
                , "Longer duration: " ++ show longerDuration
                , "Commissions (shorter): " ++ show commissionsShorter
                , "Commissions (longer): " ++ show commissionsLonger
                ])
            (commissionsLonger > commissionsShorter)

--------------------------------------------------------------------------------
