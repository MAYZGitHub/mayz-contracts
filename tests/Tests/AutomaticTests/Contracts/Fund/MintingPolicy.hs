--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4

{- |
Module      : Fund.MintingPolicy
Description : Validation logic and tests related to the Fund minting policy.

This module defines the validation logic for the Fund's minting policy.

It includes multiple test cases to ensure the integrity and correctness of the
minting script.
-}
module Contracts.Fund.MintingPolicy where

--------------------------------------------------------------------------------

-- Non-IOG imports

import           Prelude                            as P
import qualified Test.Tasty                         as Tasty

-- IOG imports

-- Project imports
import           TestUtils.Automatic.TestCases
import           TestUtils.Automatic.TestConfigTree
import           TestUtils.Automatic.TestRules
import           TestUtils.Automatic.TxGenerators
import           TestUtils.Automatic.Types
import           TestUtils.Constants
import           TestUtils.Contracts.InitialData
import           TestUtils.Contracts.TxSpecs.Fund
import           TestUtils.Types
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------

fund_Policy_Tests :: TestParams -> RuleTree -> Tasty.TestTree
fund_Policy_Tests tp ruleTree =
    Tasty.testGroup
        "Fund Policy Tests"
        [ fund_Policy_Redeemer_MintID_Tests tp ruleTree
        , fund_Policy_Redeemer_BurnID_Tests tp ruleTree
        , fund_Policy_Redeemer_MintFT_Tests tp ruleTree
        , fund_Policy_Redeemer_BurnFT_Tests tp ruleTree
        ]

--------------------------------------------------------------------------------

fund_Policy_Redeemer_MintID_Tests :: TestParams -> RuleTree -> Tasty.TestTree
fund_Policy_Redeemer_MintID_Tests tp ruleTree =
    let ------------------------
        txName = show Fund_Create_Tx
        txSpecs = fund_Create_TxSpecs tp
        defaultTestCaseParams = generateTestCaseParams txSpecs
        selectedRedeemer = RedeemerLogPolicy (Just Fund_MintID_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp txSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer txSpecs ruleTree redeemerTestConfigTree
     in ------------------------

        transaction_Tests_Gen tp selectedRedeemer txName (P.const txSpecs) [] [] updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------

fund_Policy_Redeemer_BurnID_Tests :: TestParams -> RuleTree -> Tasty.TestTree
fund_Policy_Redeemer_BurnID_Tests tp ruleTree =
    let ------------------------
        txName = show Fund_Delete_Tx
        txSpecs = fund_Delete_TxSpecs tp
        defaultTestCaseParams = generateTestCaseParams txSpecs
        selectedRedeemer = RedeemerLogPolicy (Just Fund_BurnID_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp txSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer txSpecs ruleTree redeemerTestConfigTree
     in ------------------------

        transaction_Tests_Gen tp selectedRedeemer txName (P.const txSpecs) [] [] updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------

fund_Policy_Redeemer_MintFT_Tests :: TestParams -> RuleTree -> Tasty.TestTree
fund_Policy_Redeemer_MintFT_Tests tp ruleTree =
    let ------------------------
        txName = show Fund_Deposit_Tx
        txSpecs = fund_Deposit_TxSpecs tp
        ------------
        txParams_Default =
            [ TxParam "beginDate" (tpBeginAt tp)
            , TxParam "deadlineDate" (tpDeadline tp)
            , TxParam "fundCommission_PerYear_InBPx1e3" (tpCommissionPerYearInBPx1e3 tp)
            , TxParam "depositDate" (tpDepositDate tp)
            , TxParam "investUnitTokensQty" (5 :: Integer)
            , TxParam "investUnitTokens" investUnit_Initial
            , TxParam "depositAmount" deposit_MockData
            ]
        ------------------------
        defaultTxSpecs = txSpecs txParams_Default
        defaultTestCaseParams = generateTestCaseParams defaultTxSpecs
        selectedRedeemer = RedeemerLogPolicy (Just Fund_MintFT_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp defaultTxSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer defaultTxSpecs ruleTree redeemerTestConfigTree
     in ------------------------

        --NOTE: esta transaccion solo pruebo con los valores defaults, pero la txSpecs maneja lsita de parametros
        transaction_Tests_Gen tp selectedRedeemer txName (P.const defaultTxSpecs) [] [] updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------

fund_Policy_Redeemer_BurnFT_Tests :: TestParams -> RuleTree -> Tasty.TestTree
fund_Policy_Redeemer_BurnFT_Tests tp ruleTree =
    let ------------------------
        txName = show Fund_Withdraw_Tx
        txSpecs = fund_Withdraw_TxSpecs tp
        ------------
        txParams_Default =
            [ TxParam "beginDate" (tpBeginAt tp)
            , TxParam "deadlineDate" (tpDeadline tp)
            , TxParam "fundCommission_PerYear_InBPx1e3" (tpCommissionPerYearInBPx1e3 tp)
            , TxParam "depositDate" (tpDepositDate tp)
            , TxParam "withdrawDate" (tpWithdrawDate tp)
            , TxParam "investUnitTokensQty" (5 :: Integer)
            , TxParam "investUnitTokens" investUnit_Initial
            , TxParam "depositAmount" deposit_MockData
            , TxParam "withdrawAmount" withdraw_MockData
            ]
        ------------
        defaultTxSpecs = txSpecs txParams_Default
        defaultTestCaseParams = generateTestCaseParams defaultTxSpecs
        selectedRedeemer = RedeemerLogPolicy (Just Fund_BurnFT_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp defaultTxSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer defaultTxSpecs ruleTree redeemerTestConfigTree
     in ------------------------

        --NOTE: esta transaccion se va a comprobar con todos los extras y los parametros en FundHolding, aqui solo pruebo con los valores defaults
        transaction_Tests_Gen tp selectedRedeemer txName (P.const defaultTxSpecs) [] [] updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------
