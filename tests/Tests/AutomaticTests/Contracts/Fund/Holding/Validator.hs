--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4

{- |
Module      : Fund.Holding.Validator
Description : Validation logic and tests related to the FundHolding module.

This module defines the validation logic for the FundHolding contract.

It includes multiple test cases to ensure the integrity and correctness of the
validation script.
-}
module Contracts.Fund.Holding.Validator where

--------------------------------------------------------------------------------

-- Non-IOG imports
import qualified GHC.Stack as GHC
import qualified Test.Tasty as Tasty
import Prelude as P

-- IOG imports
import qualified Plutus.V2.Ledger.Api as LedgerApiV2

-- Project imports

import qualified Generic.Constants as T
import qualified Protocol.Constants as T
import qualified Protocol.Fund.Holding.Types as FundHoldingT
import qualified Protocol.Protocol.Types as ProtocolT

import TestUtils.Automatic.ParamsGenerators
import TestUtils.Automatic.ParamsGeneratorsMAYZ
import TestUtils.Automatic.TestCases
import TestUtils.Automatic.TestConfigTree
import TestUtils.Automatic.TestRules
import TestUtils.Automatic.TxGenerators
import TestUtils.Automatic.Types
import TestUtils.Constants
import TestUtils.Contracts.InitialData
import TestUtils.Contracts.TxSpecs.FundHolding
import TestUtils.Types
import TestUtils.TypesMAYZ

--------------------------------------------------------------------------------

fundHolding_Validator_Tests :: TestParams -> RuleTree -> Tasty.TestTree
fundHolding_Validator_Tests tp ruleTree =
    Tasty.testGroup
        "FundHolding Validator Tests"
        [ fundHolding_Validator_Redeemer_UpdateMinADA_Tests tp ruleTree
        , fundHolding_Validator_Redeemer_Deposit_Tests tp ruleTree
        , fundHolding_Validator_Redeemer_Withdraw_Tests tp ruleTree
        , fundHolding_Validator_Redeemer_Collect_Protocol_Commission_Tests tp ruleTree
        , fundHolding_Validator_Redeemer_Collect_Managers_Commission_Tests tp ruleTree
        , fundHolding_Validator_Redeemer_Collect_Delegators_Commission_Tests tp ruleTree
        , fundHolding_Validator_Redeemer_ReIndexing_Tests tp ruleTree
        , fundHolding_Validator_Redeemer_BalanceAssets_Tests tp ruleTree
        , fundHolding_Validator_Redeemer_Emergency_Tests tp ruleTree
        , fundHolding_Validator_Redeemer_Delete_Tests tp ruleTree
        ]

--------------------------------------------------------------------------------

fundHolding_Validator_Redeemer_UpdateMinADA_Tests :: TestParams -> RuleTree -> Tasty.TestTree
fundHolding_Validator_Redeemer_UpdateMinADA_Tests tp ruleTree =
    let
        ------------------------
        txName = show FundHolding_UpdateMinADA_Tx
        txSpecs = fundHolding_UpdateMinADA_TxSpecs tp
        ------------
        txParams_Default =
            [ TxParam "newMinADA" (toAlter_minAda :: Integer)
            ]
        ------------
        txParamsGenerators_Valid =
            TxParamGenerators
                [ intRangeParam "newMinADA" 3_000_000 100_000_000
                ]
        txParamsGenerators_Negative =
            TxParamGenerators
                [ negativeIntParam "newMinADA"
                ]
        txParamsGenerators_List =
            [("Valid Min ADA positive", txParamsGenerators_Valid), ("Invalid Min ADA negative", txParamsGenerators_Negative)]
        ------------------------
        defaultTxSpecs = txSpecs txParams_Default
        defaultTestCaseParams = generateTestCaseParams defaultTxSpecs
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_UpdateMinADA_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp defaultTxSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer defaultTxSpecs ruleTree redeemerTestConfigTree
    in
        ------------------------
        transaction_Tests_Gen tp selectedRedeemer txName txSpecs txParams_Default txParamsGenerators_List updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------

fundHolding_Validator_Redeemer_Deposit_Tests :: TestParams -> RuleTree -> Tasty.TestTree
fundHolding_Validator_Redeemer_Deposit_Tests tp ruleTree =
    let
        ------------------------
        txName = show Fund_Deposit_Tx
        txSpecs = fundHolding_Deposit_TxSpecs tp
        ------------------------
        -- Fecha inicial, cualquier valor
        fromTime = LedgerApiV2.POSIXTime ((1 * 30 * 24 * 60 * 60 * 1000) :: Integer)
        toTime = LedgerApiV2.POSIXTime ((100 * 30 * 24 * 60 * 60 * 1000) :: Integer)
        ------------
        -- Para calcular deadline
        minLifeTime = ProtocolT.mmdMin (tpFundLifeTime tp)
        maxLifeTime = ProtocolT.mmdMax (tpFundLifeTime tp)
        ------------
        -- Para calcular deposit date
        maxError = LedgerApiV2.POSIXTime ((1 * 30 * 24 * 60 * 60 * 1000) :: Integer)
        ------------
        txParams_Default =
            [ TxParam "beginDate" (tpBeginAt tp)
            , TxParam "deadlineDate" (tpDeadline tp)
            , TxParam "fundCommission_PerYear_InBPx1e3" (tpCommission_PerYear_InBPx1e3 tp)
            , TxParam "depositDate" (tpDepositDate tp)
            , TxParam "investUnitTokensQty" (5 :: Integer)
            , TxParam "investUnitTokens" investUnit_Initial
            , TxParam "depositAmount" deposit_MockData
            ]
        ------------
        txParamsGenerators_Valid_Deposit =
            TxParamGenerators
                [ posixTimeRangeParam "beginDate" fromTime toTime
                , dependentAfterPlusPosixTimeRangeParam "deadlineDate" T.validTxTimeRange maxLifeTime "beginDate"
                , intRangeParam "fundCommission_PerYear_InBPx1e3" (ProtocolT.mmdMin $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp) (ProtocolT.mmdMax $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp)
                , dependentBetweenPlusPosixTimeRangeParam "depositDate" T.validTxTimeRange "beginDate" "deadlineDate"
                , intRangeParam "investUnitTokensQty" 1 20
                , dependentInvestUnitParam "investUnitTokens" False "investUnitTokensQty"
                , dependentDepositParam "depositAmount" 1 100_000_000 True "investUnitTokens"
                ]
        txParamsGenerators_Deposit_VerySmall =
            TxParamGenerators
                [ posixTimeRangeParam "beginDate" fromTime toTime
                , dependentAfterPlusPosixTimeRangeParam "deadlineDate" T.validTxTimeRange maxLifeTime "beginDate"
                , intRangeParam "fundCommission_PerYear_InBPx1e3" (ProtocolT.mmdMin $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp) (ProtocolT.mmdMax $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp)
                , dependentBetweenPlusPosixTimeRangeParam "depositDate" T.validTxTimeRange "beginDate" "deadlineDate"
                , intRangeParam "investUnitTokensQty" 1 20
                , dependentInvestUnitParamNoDecimals "investUnitTokens" "investUnitTokensQty"
                , dependentDepositParam "depositAmount" 1 1 True "investUnitTokens"
                ]
        txParamsGenerators_Deposit_Max =
            TxParamGenerators
                [ posixTimeRangeParam "beginDate" fromTime toTime
                , dependentAfterPlusPosixTimeRangeParam "deadlineDate" T.validTxTimeRange maxLifeTime "beginDate"
                , intRangeParam "fundCommission_PerYear_InBPx1e3" (ProtocolT.mmdMin $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp) (ProtocolT.mmdMax $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp)
                , dependentBetweenPlusPosixTimeRangeParam "depositDate" T.validTxTimeRange "beginDate" "deadlineDate"
                , intRangeParam "investUnitTokensQty" 1 20
                , dependentInvestUnitParam "investUnitTokens" False "investUnitTokensQty"
                , dependentDepositParam "depositAmount" T.maxDepositAndWithdraw_aux T.maxDepositAndWithdraw_aux True "investUnitTokens"
                ]
        txParamsGenerators_Deposit_ZeroCommission =
            TxParamGenerators
                [ posixTimeRangeParam "beginDate" fromTime toTime
                , dependentAfterPlusPosixTimeRangeParam "deadlineDate" T.validTxTimeRange maxLifeTime "beginDate"
                , intRangeParam "fundCommission_PerYear_InBPx1e3" 0 0
                , dependentBetweenPlusPosixTimeRangeParam "depositDate" T.validTxTimeRange "beginDate" "deadlineDate"
                , intRangeParam "investUnitTokensQty" 1 20
                , dependentInvestUnitParam "investUnitTokens" False "investUnitTokensQty"
                , dependentDepositParam "depositAmount" T.maxDepositAndWithdraw_aux T.maxDepositAndWithdraw_aux True "investUnitTokens"
                ]

        txParamsGenerators_Deposit_MaxCommission =
            TxParamGenerators
                [ posixTimeRangeParam "beginDate" fromTime toTime
                , dependentAfterPlusPosixTimeRangeParam "deadlineDate" T.validTxTimeRange maxLifeTime "beginDate"
                , intRangeParam "fundCommission_PerYear_InBPx1e3" 10_000_000 10_000_000
                , dependentBetweenPlusPosixTimeRangeParam "depositDate" T.validTxTimeRange "beginDate" "deadlineDate"
                , intRangeParam "investUnitTokensQty" 1 20
                , dependentInvestUnitParam "investUnitTokens" False "investUnitTokensQty"
                , dependentDepositParam "depositAmount" T.maxDepositAndWithdraw_aux T.maxDepositAndWithdraw_aux True "investUnitTokens"
                ]

        txParamsGenerators_Invalid_InvestUnit =
            TxParamGenerators
                [ posixTimeRangeParam "beginDate" fromTime toTime
                , dependentAfterPlusPosixTimeRangeParam "deadlineDate" T.validTxTimeRange maxLifeTime "beginDate"
                , intRangeParam "fundCommission_PerYear_InBPx1e3" (ProtocolT.mmdMin $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp) (ProtocolT.mmdMax $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp)
                , dependentBetweenPlusPosixTimeRangeParam "depositDate" T.validTxTimeRange "beginDate" "deadlineDate"
                , intRangeParam "investUnitTokensQty" 40 45
                , dependentInvestUnitParam "investUnitTokens" False "investUnitTokensQty"
                , dependentDepositParam "depositAmount" 1 100_000_000 True "investUnitTokens"
                ]
        txParamsGenerators_Invalid_Amount_LessZero =
            TxParamGenerators
                [ posixTimeRangeParam "beginDate" fromTime toTime
                , dependentAfterPlusPosixTimeRangeParam "deadlineDate" T.validTxTimeRange maxLifeTime "beginDate"
                , intRangeParam "fundCommission_PerYear_InBPx1e3" (ProtocolT.mmdMin $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp) (ProtocolT.mmdMax $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp)
                , dependentBetweenPlusPosixTimeRangeParam "depositDate" T.validTxTimeRange "beginDate" "deadlineDate"
                , intRangeParam "investUnitTokensQty" 1 20
                , dependentInvestUnitParam "investUnitTokens" False "investUnitTokensQty"
                , dependentDepositParam "depositAmount" 0 (-100_000_000) True "investUnitTokens"
                ]
        txParamsGenerators_Invalid_Amount_MoreMax =
            TxParamGenerators
                [ posixTimeRangeParam "beginDate" fromTime toTime
                , dependentAfterPlusPosixTimeRangeParam "deadlineDate" T.validTxTimeRange maxLifeTime "beginDate"
                , intRangeParam "fundCommission_PerYear_InBPx1e3" (ProtocolT.mmdMin $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp) (ProtocolT.mmdMax $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp)
                , dependentBetweenPlusPosixTimeRangeParam "depositDate" T.validTxTimeRange "beginDate" "deadlineDate"
                , intRangeParam "investUnitTokensQty" 1 20
                , dependentInvestUnitParam "investUnitTokens" False "investUnitTokensQty"
                , dependentDepositParam "depositAmount" (T.maxDepositAndWithdraw_aux + 1) (T.maxDepositAndWithdraw_aux + 100_000_000) True "investUnitTokens"
                ]
        txParamsGenerators_Invalid_Amount_Granularity =
            TxParamGenerators
                [ posixTimeRangeParam "beginDate" fromTime toTime
                , dependentAfterPlusPosixTimeRangeParam "deadlineDate" T.validTxTimeRange maxLifeTime "beginDate"
                , intRangeParam "fundCommission_PerYear_InBPx1e3" (ProtocolT.mmdMin $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp) (ProtocolT.mmdMax $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp)
                , dependentBetweenPlusPosixTimeRangeParam "depositDate" T.validTxTimeRange "beginDate" "deadlineDate"
                , intRangeParam "investUnitTokensQty" 1 20
                , dependentInvestUnitParam "investUnitTokens" True "investUnitTokensQty"
                , dependentDepositParam "depositAmount" 1 100_000_000 False "investUnitTokens"
                ]
        txParamsGenerators_Valid_With_MinLifeTime =
            TxParamGenerators
                [ posixTimeRangeParam "beginDate" fromTime toTime
                , dependentAfterPlusPosixTimeRangeParam "deadlineDate" T.validTxTimeRange minLifeTime "beginDate"
                , intRangeParam "fundCommission_PerYear_InBPx1e3" (ProtocolT.mmdMin $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp) (ProtocolT.mmdMax $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp)
                , dependentBetweenPlusPosixTimeRangeParam "depositDate" T.validTxTimeRange "beginDate" "deadlineDate"
                , intRangeParam "investUnitTokensQty" 1 20
                , dependentInvestUnitParam "investUnitTokens" False "investUnitTokensQty"
                , dependentDepositParam "depositAmount" 1 100_000_000 True "investUnitTokens"
                ]

        txParamsGenerators_Invalid_DepositDate_TooEarly =
            TxParamGenerators
                [ posixTimeRangeParam "beginDate" fromTime toTime
                , dependentAfterPlusPosixTimeRangeParam "deadlineDate" T.validTxTimeRange maxLifeTime "beginDate"
                , intRangeParam "fundCommission_PerYear_InBPx1e3" (ProtocolT.mmdMin $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp) (ProtocolT.mmdMax $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp)
                , dependentBeforePosixTimeRangeParam "depositDate" maxError "beginDate"
                , intRangeParam "investUnitTokensQty" 1 20
                , dependentInvestUnitParam "investUnitTokens" False "investUnitTokensQty"
                , dependentDepositParam "depositAmount" 1 100_000_000 True "investUnitTokens"
                ]
        txParamsGenerators_Invalid_DepositDate_TooLate =
            TxParamGenerators
                [ posixTimeRangeParam "beginDate" fromTime toTime
                , dependentAfterPlusPosixTimeRangeParam "deadlineDate" T.validTxTimeRange maxLifeTime "beginDate"
                , intRangeParam "fundCommission_PerYear_InBPx1e3" (ProtocolT.mmdMin $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp) (ProtocolT.mmdMax $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp)
                , dependentAfterPosixTimeRangeParam "depositDate" maxError "deadlineDate"
                , intRangeParam "investUnitTokensQty" 1 20
                , dependentInvestUnitParam "investUnitTokens" False "investUnitTokensQty"
                , dependentDepositParam "depositAmount" 1 100_000_000 True "investUnitTokens"
                ]
        txParamsGenerators_List =
            [ ("Valid Deposit", txParamsGenerators_Valid_Deposit)
            , ("Deposit very small", txParamsGenerators_Deposit_VerySmall)
            , ("Deposit max", txParamsGenerators_Deposit_Max)
            , ("Deposit with 0% commission rate", txParamsGenerators_Deposit_ZeroCommission)
            , ("Deposit with max commission rate", txParamsGenerators_Deposit_MaxCommission)
            , ("Invalid quantity of Tokens in Invest Unit", txParamsGenerators_Invalid_InvestUnit)
            , ("Invalid Deposit amount <= 0", txParamsGenerators_Invalid_Amount_LessZero)
            , ("Invalid Deposit amount > MAX", txParamsGenerators_Invalid_Amount_MoreMax)
            , ("Invalid Deposit amount not multiplier of Invest Unit granularity", txParamsGenerators_Invalid_Amount_Granularity)
            , ("Valid Deposit with MinLifeTime", txParamsGenerators_Valid_With_MinLifeTime)
            , ("Invalid DepositDate too early", txParamsGenerators_Invalid_DepositDate_TooEarly)
            , ("Invalid DepositDate too late", txParamsGenerators_Invalid_DepositDate_TooLate)
            ]
        {-
        Granularity Explanation:

        In this context, granularity refers to the smallest fractional unit that can be represented
        in an InvestUnit token. It's used to simulate different decimal place precisions for tokens.

        1. Granularity Levels:
        - 1: Represents whole numbers (e.g., 1, 2, 3)
        - 10: Represents one decimal place (e.g., 0.1, 0.2, 1.5)
        - 100: Represents two decimal places (e.g., 0.01, 0.55, 1.23)

        2. How it's used:
        - In genInvestUnitToken:
            When useForcedGranularity is true, token amounts are generated as:
            (base * granularity) + offset
            This ensures that the amount has a specific granularity (10 or 100),
            simulating tokens with 1 or 2 decimal places.

        - In getDecimalsInInvestUnit:
            This function determines the highest precision (smallest fraction)
            used across all tokens in an InvestUnit.

        - In dependentDepositParam:
            The deposit amount is generated to either match or intentionally mismatch
            the InvestUnit's granularity, based on the validGranularity flag.

        3. Importance:
        Granularity ensures that generated amounts are realistic and consistent
        with the precision of the InvestUnit tokens. This is crucial for testing
        scenarios where exact divisibility is required for operations like
        deposits and withdrawals.

        4. Example:
        If an InvestUnit has tokens with amounts like 455 and 1000 (representing 4.55 and 10.00),
        the granularity would be 100. Valid deposit amounts would need to be multiples of 100
        to ensure they can be evenly distributed across all tokens in the InvestUnit.
        -}
        ------------------------
        defaultTxSpecs = txSpecs txParams_Default
        defaultTestCaseParams = generateTestCaseParams defaultTxSpecs
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_Deposit_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp defaultTxSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer defaultTxSpecs ruleTree redeemerTestConfigTree
    in
        ------------------------

        transaction_Tests_Gen tp selectedRedeemer txName txSpecs txParams_Default txParamsGenerators_List updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------

fundHolding_Validator_Redeemer_Withdraw_Tests :: GHC.HasCallStack => TestParams -> RuleTree -> Tasty.TestTree
fundHolding_Validator_Redeemer_Withdraw_Tests tp ruleTree =
    let
        ------------------------
        txName = show Fund_Withdraw_Tx
        txSpecs = fundHolding_Withdraw_TxSpecs tp
        ------------------------
        -- Fecha inicial, cualquier valor
        fromTime = LedgerApiV2.POSIXTime ((1 * 30 * 24 * 60 * 60 * 1000) :: Integer)
        toTime = LedgerApiV2.POSIXTime ((100 * 30 * 24 * 60 * 60 * 1000) :: Integer)
        ------------
        -- Para calcular deadline
        minLifeTime = ProtocolT.mmdMin (tpFundLifeTime tp)
        maxLifeTime = ProtocolT.mmdMax (tpFundLifeTime tp)
        ------------
        -- Para calcular withdraw date
        maxError = LedgerApiV2.POSIXTime ((1 * 30 * 24 * 60 * 60 * 1000) :: Integer)
        ------------
        txParams_Default =
            [ TxParam "beginDate" (tpBeginAt tp)
            , TxParam "deadlineDate" (tpDeadline tp)
            , TxParam "fundCommission_PerYear_InBPx1e3" (tpCommission_PerYear_InBPx1e3 tp)
            , TxParam "depositDate" (tpDepositDate tp)
            , TxParam "withdrawDate" (tpWithdrawDate tp)
            , TxParam "investUnitTokensQty" (5 :: Integer)
            , TxParam "investUnitTokens" investUnit_Initial
            , TxParam "depositAmount" deposit_MockData
            , TxParam "withdrawAmount" withdraw_MockData
            ]
        ------------
        txParamsGenerators_Valid_Withdraw =
            TxParamGenerators
                [ posixTimeRangeParam "beginDate" fromTime toTime
                , dependentAfterPlusPosixTimeRangeParam "deadlineDate" T.validTxTimeRange maxLifeTime "beginDate"
                , intRangeParam "fundCommission_PerYear_InBPx1e3" (ProtocolT.mmdMin $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp) (ProtocolT.mmdMax $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp)
                , dependentBetweenPlusPosixTimeRangeParam "depositDate" T.validTxTimeRange "beginDate" "deadlineDate"
                , dependentBetweenPlusPosixTimeRangeParam "withdrawDate" T.validTxTimeRange "depositDate" "deadlineDate"
                , intRangeParam "investUnitTokensQty" 1 20
                , dependentInvestUnitParam "investUnitTokens" False "investUnitTokensQty"
                , dependentDepositParam "depositAmount" 1 100_000_000 True "investUnitTokens"
                , dependentWithdrawParam "withdrawAmount" 1 100_000_000 True True "investUnitTokens" "depositAmount" "beginDate" "deadlineDate" "depositDate" "fundCommission_PerYear_InBPx1e3"
                ]

        txParamsGenerators_Withdraw_VerySmall =
            TxParamGenerators
                [ posixTimeRangeParam "beginDate" fromTime toTime
                , dependentAfterPlusPosixTimeRangeParam "deadlineDate" T.validTxTimeRange maxLifeTime "beginDate"
                , intRangeParam "fundCommission_PerYear_InBPx1e3" (ProtocolT.mmdMin $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp) (ProtocolT.mmdMax $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp)
                , dependentBetweenPlusPosixTimeRangeParam "depositDate" T.validTxTimeRange "beginDate" "deadlineDate"
                , dependentBetweenPlusPosixTimeRangeParam "withdrawDate" T.validTxTimeRange "depositDate" "deadlineDate"
                , intRangeParam "investUnitTokensQty" 1 20
                , dependentInvestUnitParamNoDecimals "investUnitTokens" "investUnitTokensQty"
                , dependentDepositParam "depositAmount" 1 100_000_000 True "investUnitTokens"
                , dependentWithdrawParam "withdrawAmount" 1 1 True True "investUnitTokens" "depositAmount" "beginDate" "deadlineDate" "depositDate" "fundCommission_PerYear_InBPx1e3"
                ]
        txParamsGenerators_Withdraw_Max =
            TxParamGenerators
                [ posixTimeRangeParam "beginDate" fromTime toTime
                , dependentAfterPlusPosixTimeRangeParam "deadlineDate" T.validTxTimeRange maxLifeTime "beginDate"
                , intRangeParam "fundCommission_PerYear_InBPx1e3" (ProtocolT.mmdMin $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp) (ProtocolT.mmdMax $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp)
                , dependentBetweenPlusPosixTimeRangeParam "depositDate" T.validTxTimeRange "beginDate" "deadlineDate"
                , dependentBetweenPlusPosixTimeRangeParam "withdrawDate" T.validTxTimeRange "depositDate" "deadlineDate"
                , intRangeParam "investUnitTokensQty" 1 20
                , dependentInvestUnitParam "investUnitTokens" False "investUnitTokensQty"
                , dependentDepositParam "depositAmount" T.maxDepositAndWithdraw_aux T.maxDepositAndWithdraw_aux True "investUnitTokens"
                , dependentWithdrawParam "withdrawAmount" 0 0 True False "investUnitTokens" "depositAmount" "beginDate" "deadlineDate" "depositDate" "fundCommission_PerYear_InBPx1e3"
                ]
        txParamsGenerators_Withdraw_ZeroCommission =
            TxParamGenerators
                [ posixTimeRangeParam "beginDate" fromTime toTime
                , dependentAfterPlusPosixTimeRangeParam "deadlineDate" T.validTxTimeRange maxLifeTime "beginDate"
                , intRangeParam "fundCommission_PerYear_InBPx1e3" 0 0
                , dependentBetweenPlusPosixTimeRangeParam "depositDate" T.validTxTimeRange "beginDate" "deadlineDate"
                , dependentBetweenPlusPosixTimeRangeParam "withdrawDate" T.validTxTimeRange "depositDate" "deadlineDate"
                , intRangeParam "investUnitTokensQty" 1 20
                , dependentInvestUnitParam "investUnitTokens" False "investUnitTokensQty"
                , dependentDepositParam "depositAmount" 1 100_000_000 True "investUnitTokens"
                , dependentWithdrawParam "withdrawAmount" 1 100_000_000 True True "investUnitTokens" "depositAmount" "beginDate" "deadlineDate" "depositDate" "fundCommission_PerYear_InBPx1e3"
                ]

        txParamsGenerators_Withdraw_MaxCommission =
            TxParamGenerators
                [ posixTimeRangeParam "beginDate" fromTime toTime
                , dependentAfterPlusPosixTimeRangeParam "deadlineDate" T.validTxTimeRange maxLifeTime "beginDate"
                , intRangeParam "fundCommission_PerYear_InBPx1e3" 10_000_000 10_000_000
                , dependentBetweenPlusPosixTimeRangeParam "depositDate" T.validTxTimeRange "beginDate" "deadlineDate"
                , dependentBetweenPlusPosixTimeRangeParam "withdrawDate" T.validTxTimeRange "depositDate" "deadlineDate"
                , intRangeParam "investUnitTokensQty" 1 20
                , dependentInvestUnitParam "investUnitTokens" False "investUnitTokensQty"
                , dependentDepositParam "depositAmount" 1 100_000_000 True "investUnitTokens"
                , dependentWithdrawParam "withdrawAmount" 1 100_000_000 True True "investUnitTokens" "depositAmount" "beginDate" "deadlineDate" "depositDate" "fundCommission_PerYear_InBPx1e3"
                ]

        txParamsGenerators_Invalid_InvestUnit =
            TxParamGenerators
                [ posixTimeRangeParam "beginDate" fromTime toTime
                , dependentAfterPlusPosixTimeRangeParam "deadlineDate" T.validTxTimeRange maxLifeTime "beginDate"
                , intRangeParam "fundCommission_PerYear_InBPx1e3" (ProtocolT.mmdMin $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp) (ProtocolT.mmdMax $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp)
                , dependentBetweenPlusPosixTimeRangeParam "depositDate" T.validTxTimeRange "beginDate" "deadlineDate"
                , dependentBetweenPlusPosixTimeRangeParam "withdrawDate" T.validTxTimeRange "depositDate" "deadlineDate"
                , intRangeParam "investUnitTokensQty" 35 45
                , dependentInvestUnitParam "investUnitTokens" False "investUnitTokensQty"
                , dependentDepositParam "depositAmount" 1 100_000_000 True "investUnitTokens"
                , dependentWithdrawParam "withdrawAmount" 1 100_000_000 True True "investUnitTokens" "depositAmount" "beginDate" "deadlineDate" "depositDate" "fundCommission_PerYear_InBPx1e3"
                ]

        txParamsGenerators_Invalid_Amount_LessZero =
            TxParamGenerators
                [ posixTimeRangeParam "beginDate" fromTime toTime
                , dependentAfterPlusPosixTimeRangeParam "deadlineDate" T.validTxTimeRange maxLifeTime "beginDate"
                , intRangeParam "fundCommission_PerYear_InBPx1e3" (ProtocolT.mmdMin $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp) (ProtocolT.mmdMax $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp)
                , dependentBetweenPlusPosixTimeRangeParam "depositDate" T.validTxTimeRange "beginDate" "deadlineDate"
                , dependentBetweenPlusPosixTimeRangeParam "withdrawDate" T.validTxTimeRange "depositDate" "deadlineDate"
                , intRangeParam "investUnitTokensQty" 1 20
                , dependentInvestUnitParam "investUnitTokens" False "investUnitTokensQty"
                , dependentDepositParam "depositAmount" 1 100_000_000 True "investUnitTokens"
                , dependentWithdrawParam "withdrawAmount" (-100_000_000) 0 True True "investUnitTokens" "depositAmount" "beginDate" "deadlineDate" "depositDate" "fundCommission_PerYear_InBPx1e3"
                ]

        txParamsGenerators_Invalid_Amount_MoreMax =
            TxParamGenerators
                [ posixTimeRangeParam "beginDate" fromTime toTime
                , dependentAfterPlusPosixTimeRangeParam "deadlineDate" T.validTxTimeRange maxLifeTime "beginDate"
                , intRangeParam "fundCommission_PerYear_InBPx1e3" (ProtocolT.mmdMin $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp) (ProtocolT.mmdMax $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp)
                , dependentBetweenPlusPosixTimeRangeParam "depositDate" T.validTxTimeRange "beginDate" "deadlineDate"
                , dependentBetweenPlusPosixTimeRangeParam "withdrawDate" T.validTxTimeRange "depositDate" "deadlineDate"
                , intRangeParam "investUnitTokensQty" 1 20
                , dependentInvestUnitParam "investUnitTokens" False "investUnitTokensQty"
                , dependentDepositParam "depositAmount" 1 100_000_000 True "investUnitTokens"
                , dependentWithdrawParam "withdrawAmount" (T.maxDepositAndWithdraw_aux + 1) (T.maxDepositAndWithdraw_aux + 100_000_000) True False "investUnitTokens" "depositAmount" "beginDate" "deadlineDate" "depositDate" "fundCommission_PerYear_InBPx1e3"
                ]

        txParamsGenerators_Invalid_Amount_Granularity =
            TxParamGenerators
                [ posixTimeRangeParam "beginDate" fromTime toTime
                , dependentAfterPlusPosixTimeRangeParam "deadlineDate" T.validTxTimeRange maxLifeTime "beginDate"
                , intRangeParam "fundCommission_PerYear_InBPx1e3" (ProtocolT.mmdMin $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp) (ProtocolT.mmdMax $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp)
                , dependentBetweenPlusPosixTimeRangeParam "depositDate" T.validTxTimeRange "beginDate" "deadlineDate"
                , dependentBetweenPlusPosixTimeRangeParam "withdrawDate" T.validTxTimeRange "depositDate" "deadlineDate"
                , intRangeParam "investUnitTokensQty" 1 20
                , dependentInvestUnitParam "investUnitTokens" True "investUnitTokensQty"
                , dependentDepositParam "depositAmount" 1 100_000_000 True "investUnitTokens"
                , dependentWithdrawParam "withdrawAmount" 1 100_000_000 False True "investUnitTokens" "depositAmount" "beginDate" "deadlineDate" "depositDate" "fundCommission_PerYear_InBPx1e3"
                ]

        txParamsGenerators_Valid_With_MinLifeTime =
            TxParamGenerators
                [ posixTimeRangeParam "beginDate" fromTime toTime
                , dependentAfterPlusPosixTimeRangeParam "deadlineDate" minLifeTime T.validTxTimeRange "beginDate"
                , intRangeParam "fundCommission_PerYear_InBPx1e3" (ProtocolT.mmdMin $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp) (ProtocolT.mmdMax $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp)
                , dependentBetweenPlusPosixTimeRangeParam "depositDate" T.validTxTimeRange "beginDate" "deadlineDate"
                , dependentBetweenPlusPosixTimeRangeParam "withdrawDate" T.validTxTimeRange "depositDate" "deadlineDate"
                , intRangeParam "investUnitTokensQty" 1 20
                , dependentInvestUnitParam "investUnitTokens" False "investUnitTokensQty"
                , dependentDepositParam "depositAmount" 1 100_000_000 True "investUnitTokens"
                , dependentWithdrawParam "withdrawAmount" 1 100_000_000 True True "investUnitTokens" "depositAmount" "beginDate" "deadlineDate" "depositDate" "fundCommission_PerYear_InBPx1e3"
                ]

        txParamsGenerators_Valid_WithdrawDate_TooLate =
            TxParamGenerators
                [ posixTimeRangeParam "beginDate" fromTime toTime
                , dependentAfterPlusPosixTimeRangeParam "deadlineDate" T.validTxTimeRange maxLifeTime "beginDate"
                , intRangeParam "fundCommission_PerYear_InBPx1e3" (ProtocolT.mmdMin $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp) (ProtocolT.mmdMax $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp)
                , dependentBetweenPlusPosixTimeRangeParam "depositDate" T.validTxTimeRange "beginDate" "deadlineDate"
                , dependentAfterPosixTimeRangeParam "withdrawDate" maxError "deadlineDate"
                , intRangeParam "investUnitTokensQty" 1 20
                , dependentInvestUnitParam "investUnitTokens" False "investUnitTokensQty"
                , dependentDepositParam "depositAmount" 1 100_000_000 True "investUnitTokens"
                , dependentWithdrawParam "withdrawAmount" 1 100_000_000 True True "investUnitTokens" "depositAmount" "beginDate" "deadlineDate" "depositDate" "fundCommission_PerYear_InBPx1e3"
                ]

        txParamsGenerators_Invalid_Withdraw_Commissions =
            -- quiero que siempre quede al menos un mes de vida, para que haya comissiones, y poner un numero mayor
            -- si quedan cero meses, comissiones es cero, y pasa bien
            -- por que NO HAY CONTROLES del amount total de withdraw... ese control va a venir solo de los tokens que realmente se encuentren en la UTXO
            -- el contrato en si no controla eso
            let
                beginDate = LedgerApiV2.POSIXTime ((1 * 30 * 24 * 60 * 60 * 1000) :: Integer)
                deadlineDate = LedgerApiV2.POSIXTime ((100 * 30 * 24 * 60 * 60 * 1000) :: Integer)
                depositDate = LedgerApiV2.POSIXTime ((10 * 30 * 24 * 60 * 60 * 1000) :: Integer)
                withdrawDate = LedgerApiV2.POSIXTime ((20 * 30 * 24 * 60 * 60 * 1000) :: Integer)
            in
                TxParamGenerators
                    [ posixTimeRangeParam "beginDate" beginDate beginDate
                    , posixTimeRangeParam "deadlineDate" deadlineDate deadlineDate
                    , intRangeParam "fundCommission_PerYear_InBPx1e3" (ProtocolT.mmdMin $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp) (ProtocolT.mmdMax $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp)
                    , posixTimeRangeParam "depositDate" depositDate depositDate
                    , posixTimeRangeParam "withdrawDate" withdrawDate withdrawDate
                    , intRangeParam "investUnitTokensQty" 1 20
                    , dependentInvestUnitParam "investUnitTokens" False "investUnitTokensQty"
                    , dependentDepositParam "depositAmount" 100 100 True "investUnitTokens"
                    , dependentWithdrawParam "withdrawAmount" 101 100_000_000 True False "investUnitTokens" "depositAmount" "beginDate" "deadlineDate" "depositDate" "fundCommission_PerYear_InBPx1e3"
                    ]

        txParamsGenerators_Invalid_WithdrawAmount_MoreThanDeposit =
            TxParamGenerators
                [ posixTimeRangeParam "beginDate" fromTime toTime
                , dependentAfterPlusPosixTimeRangeParam "deadlineDate" T.validTxTimeRange maxLifeTime "beginDate"
                , intRangeParam "fundCommission_PerYear_InBPx1e3" (ProtocolT.mmdMin $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp) (ProtocolT.mmdMax $ tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp)
                , dependentBetweenPlusPosixTimeRangeParam "depositDate" T.validTxTimeRange "beginDate" "deadlineDate"
                , dependentBetweenPlusPosixTimeRangeParam "withdrawDate" T.validTxTimeRange "depositDate" "deadlineDate"
                , intRangeParam "investUnitTokensQty" 1 20
                , dependentInvestUnitParam "investUnitTokens" False "investUnitTokensQty"
                , dependentDepositParam "depositAmount" 1 100_000_000 True "investUnitTokens"
                , dependentWithdrawParam "withdrawAmount" 1 100_000_000 True False "investUnitTokens" "depositAmount" "beginDate" "deadlineDate" "depositDate" "fundCommission_PerYear_InBPx1e3"
                ]

        txParamsGenerators_List =
            [ ("Valid Withdraw", txParamsGenerators_Valid_Withdraw)
            , ("Withdraw very small", txParamsGenerators_Withdraw_VerySmall)
            , ("Withdraw max", txParamsGenerators_Withdraw_Max)
            , ("Withdraw with 0% commission rate", txParamsGenerators_Withdraw_ZeroCommission)
            , ("Withdraw with max commission rate", txParamsGenerators_Withdraw_MaxCommission)
            , ("Invalid quantity of Tokens in Invest Unit", txParamsGenerators_Invalid_InvestUnit)
            , ("Invalid Withdraw amount <= 0", txParamsGenerators_Invalid_Amount_LessZero)
            , ("Invalid Withdraw amount > MAX", txParamsGenerators_Invalid_Amount_MoreMax)
            , ("Invalid Withdraw amount not multiplier of Invest Unit granularity", txParamsGenerators_Invalid_Amount_Granularity)
            , ("Valid Withdraw with MinLifeTime", txParamsGenerators_Valid_With_MinLifeTime)
            , ("Valid WithdrawDate too late", txParamsGenerators_Valid_WithdrawDate_TooLate)
            , ("Invalid Withdraw Commissions amount", txParamsGenerators_Invalid_Withdraw_Commissions)
            , ("Invalid Withdraw amount more than deposit", txParamsGenerators_Invalid_WithdrawAmount_MoreThanDeposit)
            ]
        ------------------------
        defaultTxSpecs = txSpecs txParams_Default
        defaultTestCaseParams = generateTestCaseParams defaultTxSpecs
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_Withdraw_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp defaultTxSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer defaultTxSpecs ruleTree redeemerTestConfigTree
    in
        ------------------------

        transaction_Tests_Gen tp selectedRedeemer txName txSpecs txParams_Default txParamsGenerators_List updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------

fundHolding_Validator_Redeemer_Collect_Protocol_Commission_Tests :: TestParams -> RuleTree -> Tasty.TestTree
fundHolding_Validator_Redeemer_Collect_Protocol_Commission_Tests tp ruleTree =
    let
        ------------------------
        txName = show FundHolding_Collect_Protocol_Commission_Tx
        txSpecs = fundHolding_Collect_Protocol_Commission_TxSpecs tp
        ------------
        txParams_Default =
            []
        ------------
        txParamsGenerators_List =
            []
        ------------------------
        defaultTxSpecs = txSpecs txParams_Default
        defaultTestCaseParams = generateTestCaseParams defaultTxSpecs
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_Collect_Protocol_Commission_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp defaultTxSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer defaultTxSpecs ruleTree redeemerTestConfigTree
    in
        ------------------------

        transaction_Tests_Gen tp selectedRedeemer txName txSpecs txParams_Default txParamsGenerators_List updatedTestConfigTree defaultTestCaseParams

fundHolding_Validator_Redeemer_Collect_Managers_Commission_Tests :: TestParams -> RuleTree -> Tasty.TestTree
fundHolding_Validator_Redeemer_Collect_Managers_Commission_Tests tp ruleTree =
    let
        ------------------------
        txName = show FundHolding_Collect_Managers_Commission_Tx
        txSpecs = fundHolding_Collect_Managers_Commission_TxSpecs tp
        ------------
        txParams_Default =
            []
        ------------
        txParamsGenerators_List =
            []
        ------------------------
        defaultTxSpecs = txSpecs txParams_Default
        defaultTestCaseParams = generateTestCaseParams defaultTxSpecs
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_Collect_Managers_Commission_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp defaultTxSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer defaultTxSpecs ruleTree redeemerTestConfigTree
    in
        ------------------------

        transaction_Tests_Gen tp selectedRedeemer txName txSpecs txParams_Default txParamsGenerators_List updatedTestConfigTree defaultTestCaseParams

fundHolding_Validator_Redeemer_Collect_Delegators_Commission_Tests :: TestParams -> RuleTree -> Tasty.TestTree
fundHolding_Validator_Redeemer_Collect_Delegators_Commission_Tests tp ruleTree =
    let
        ------------------------
        txName = show FundHolding_Collect_Delegators_Commission_Tx
        txSpecs = fundHolding_Collect_Delegators_Commission_TxSpecs tp
        ------------
        txParams_Default =
            []
        ------------
        txParamsGenerators_List =
            []
        ------------------------
        defaultTxSpecs = txSpecs txParams_Default
        defaultTestCaseParams = generateTestCaseParams defaultTxSpecs
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_Collect_Delegators_Commission_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp defaultTxSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer defaultTxSpecs ruleTree redeemerTestConfigTree
    in
        ------------------------

        transaction_Tests_Gen tp selectedRedeemer txName txSpecs txParams_Default txParamsGenerators_List updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------

fundHolding_Validator_Redeemer_ReIndexing_Tests :: TestParams -> RuleTree -> Tasty.TestTree
fundHolding_Validator_Redeemer_ReIndexing_Tests tp ruleTree =
    let
        ------------------------
        txName = show Fund_ReIndexing_Tx
        txSpecs = fundHolding_ReIndexing_TxSpecs tp
        ------------
        txParams_Default =
            []
        ------------
        txParamsGenerators_List =
            []
        ------------------------
        defaultTxSpecs = txSpecs txParams_Default
        defaultTestCaseParams = generateTestCaseParams defaultTxSpecs
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_ReIndexing_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp defaultTxSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer defaultTxSpecs ruleTree redeemerTestConfigTree
    in
        ------------------------

        transaction_Tests_Gen tp selectedRedeemer txName txSpecs txParams_Default txParamsGenerators_List updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------

fundHolding_Validator_Redeemer_BalanceAssets_Tests :: TestParams -> RuleTree -> Tasty.TestTree
fundHolding_Validator_Redeemer_BalanceAssets_Tests tp ruleTree =
    let
        ------------------------
        txName = show FundHolding_BalanceAssets_Tx
        txSpecs = fundHolding_BalanceAssets_TxSpecs tp
        ------------
        txParams_Default =
            []
        ------------
        txParamsGenerators_List =
            []
        ------------------------
        defaultTxSpecs = txSpecs txParams_Default
        defaultTestCaseParams = generateTestCaseParams defaultTxSpecs
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_BalanceAssets_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp defaultTxSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer defaultTxSpecs ruleTree redeemerTestConfigTree
    in
        ------------------------

        transaction_Tests_Gen tp selectedRedeemer txName txSpecs txParams_Default txParamsGenerators_List updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------

fundHolding_Validator_Redeemer_Emergency_Tests :: TestParams -> RuleTree -> Tasty.TestTree
fundHolding_Validator_Redeemer_Emergency_Tests tp _ =
    let
        ------------------------
        txName = show FundHolding_Emergency_Tx
        txSpecs = fundHolding_UpdateMinADA_TxSpecs tp
        ------------------------
        txParams_Default =
            [ TxParam "newMinADA" (toAlter_minAda :: Integer)
            ]
        ------------------------
        defaultTxSpecs = txSpecs txParams_Default
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_Emergency_TestRedeemer)
    in
        ------------------------

        adminTokens_Tests_Gen tp txName selectedRedeemer (P.const defaultTxSpecs) FundHoldingT.mkEmergencyRedeemer (tpTokenAdminPolicy_CS tp) (tpTokenEmergencyAdminPolicy_CS tp) T.protocolTokenAdmin_TN T.protocolTokenEmergencyAdmin_TN True True

------------------------------------------------------------------------------

fundHolding_Validator_Redeemer_Delete_Tests :: TestParams -> RuleTree -> Tasty.TestTree
fundHolding_Validator_Redeemer_Delete_Tests tp ruleTree =
    let
        ------------------------
        txName = show FundHolding_Delete_Tx
        txSpecs = fundHolding_Delete_TxSpecs tp
        defaultTestCaseParams = generateTestCaseParams txSpecs
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_Delete_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp txSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer txSpecs ruleTree redeemerTestConfigTree
    in
        ------------------------

        transaction_Tests_Gen tp selectedRedeemer txName (P.const txSpecs) [] [] updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------
