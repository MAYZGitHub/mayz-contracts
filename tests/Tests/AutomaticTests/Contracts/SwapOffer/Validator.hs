--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4

{- |
Module      : SwapOffer.Validator
Description : Validation logic and tests related to the SwapOffer validator.

This module defines the validation logic for the SwapOffer's validator

It includes multiple test cases to ensure the integrity and correctness of the
validator script.
-}
module Contracts.SwapOffer.Validator where

--------------------------------------------------------------------------------

-- Non-IOG imports

import           Prelude                               as P hiding (Bool (True), Integer, Maybe (Just), (*), (-))
import qualified Test.Tasty                            as Tasty

-- IOG imports
import           PlutusTx.Prelude


-- Project imports

import qualified Protocol.Constants                    as T
import qualified Protocol.SwapOffer.Types              as SwapOfferT
import           TestUtils.Automatic.ParamsGenerators
import           TestUtils.Automatic.TestCases
import           TestUtils.Automatic.TestConfigTree
import           TestUtils.Automatic.TestRules
import           TestUtils.Automatic.TxGenerators
import           TestUtils.Automatic.Types
import           TestUtils.Constants
import           TestUtils.Contracts.InitialData
import           TestUtils.Contracts.TxSpecs.SwapOffer
import           TestUtils.Types
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------3

swapOffer_Validator_Tests :: TestParams -> RuleTree -> Tasty.TestTree
swapOffer_Validator_Tests tp ruleTree =
    Tasty.testGroup
        "SwapOffer Validator Tests"
        [ swapOffer_Validator_Redeemer_UpdateStatus_Tests tp ruleTree
        , swapOffer_Validator_Redeemer_UpdateAskedCommissionRate_Tests tp ruleTree
        , swapOffer_Validator_Redeemer_UpdateSellRestrictions_Tests tp ruleTree
        , swapOffer_Validator_Redeemer_UpdateMinADA_Tests tp ruleTree
        , swapOffer_Validator_Redeemer_Deposit_Tests tp ruleTree
        , swapOffer_Validator_Redeemer_Withdraw_Tests tp ruleTree
        , swapOffer_Validator_Redeemer_SwapFTxADA_Tests tp ruleTree
        , swapOffer_Validator_Redeemer_SwapADAxFT_Tests tp ruleTree
        , swapOffer_Validator_Redeemer_Delete_Tests tp ruleTree
        , swapOffer_Validator_Redeemer_Emergency_Tests tp ruleTree
        ]

--------------------------------------------------------------------------------

swapOffer_Validator_Redeemer_UpdateStatus_Tests :: TestParams -> RuleTree -> Tasty.TestTree
swapOffer_Validator_Redeemer_UpdateStatus_Tests tp ruleTree =
    let ------------------------
        txName = show SwapOffer_UpdateStatus_Tx
        txSpecs = swapOffer_UpdateStatus_TxSpecs tp
        defaultTestCaseParams = generateTestCaseParams txSpecs
        selectedRedeemer = RedeemerLogValidator (Just SwapOffer_UpdateStatus_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp txSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer txSpecs ruleTree redeemerTestConfigTree
     in ------------------------

        transaction_Tests_Gen tp selectedRedeemer txName (P.const txSpecs) [] [] updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------

swapOffer_Validator_Redeemer_UpdateAskedCommissionRate_Tests :: TestParams -> RuleTree -> Tasty.TestTree
swapOffer_Validator_Redeemer_UpdateAskedCommissionRate_Tests tp ruleTree =
    let ------------------------
        txName = show SwapOffer_UpdateAskedCommissionRate_Tx
        txSpecs = swapOffer_UpdateAskedCommissionRate_TxSpecs tp
        defaultTestCaseParams = generateTestCaseParams txSpecs
        selectedRedeemer = RedeemerLogValidator (Just SwapOffer_UpdateAskedCommissionRate_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp txSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer txSpecs ruleTree redeemerTestConfigTree
     in ------------------------

        transaction_Tests_Gen tp selectedRedeemer txName (P.const txSpecs) [] [] updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------

swapOffer_Validator_Redeemer_UpdateSellRestrictions_Tests :: TestParams -> RuleTree -> Tasty.TestTree
swapOffer_Validator_Redeemer_UpdateSellRestrictions_Tests tp ruleTree =
    let ------------------------
        txName = show SwapOffer_UpdateSellRestrictions_Tx
        txSpecs = swapOffer_UpdateSellRestrictions_TxSpecs tp
        defaultTestCaseParams = generateTestCaseParams txSpecs
        selectedRedeemer = RedeemerLogValidator (Just SwapOffer_UpdateSellRestrictions_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp txSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer txSpecs ruleTree redeemerTestConfigTree
     in ------------------------
        transaction_Tests_Gen tp selectedRedeemer txName (P.const txSpecs) [] [] updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------

swapOffer_Validator_Redeemer_UpdateMinADA_Tests :: TestParams -> RuleTree -> Tasty.TestTree
swapOffer_Validator_Redeemer_UpdateMinADA_Tests tp ruleTree =
    let ------------------------
        txName = show SwapOffer_UpdateMinADA_Tx
        txSpecs = swapOffer_UpdateMinADA_TxSpecs tp
        ------------
        txParams_Default =
                [
                    TxParam "newMinADA" (toAlter_minAda::Integer),
                    TxParam "availableADA" (5_000_000::Integer)
                ]
        ------------
        txParamsGenerators_Valid =
            TxParamGenerators
                [
                    intRangeParam "newMinADA" 3_000_000 100_000_000,
                    positiveIntParam "availableADA"
                ]
        txParamsGenerators_Negative =
            TxParamGenerators
                [
                    negativeIntParam "newMinADA",
                    positiveIntParam "availableADA"
                ]
        txParamsGenerators_List =
            [("Valid Min ADA positive", txParamsGenerators_Valid), ("Invalid Min ADA negative", txParamsGenerators_Negative)]
        ------------------------
        defaultTxSpecs = txSpecs txParams_Default
        defaultTestCaseParams = generateTestCaseParams defaultTxSpecs
        selectedRedeemer = RedeemerLogValidator (Just SwapOffer_UpdateMinADA_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp defaultTxSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer defaultTxSpecs ruleTree redeemerTestConfigTree
     in ------------------------

        transaction_Tests_Gen tp selectedRedeemer txName txSpecs txParams_Default txParamsGenerators_List updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------

swapOffer_Validator_Redeemer_Deposit_Tests :: TestParams -> RuleTree -> Tasty.TestTree
swapOffer_Validator_Redeemer_Deposit_Tests tp ruleTree =
    let ------------------------
        txName = show SwapOffer_Deposit_Tx
        txSpecs = swapOffer_Deposit_TxSpecs tp 100 100
        defaultTestCaseParams = generateTestCaseParams txSpecs
        selectedRedeemer = RedeemerLogValidator (Just SwapOffer_Deposit_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp txSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer txSpecs ruleTree redeemerTestConfigTree
     in ------------------------

        transaction_Tests_Gen tp selectedRedeemer txName (P.const txSpecs) [] [] updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------

swapOffer_Validator_Redeemer_Withdraw_Tests :: TestParams -> RuleTree -> Tasty.TestTree
swapOffer_Validator_Redeemer_Withdraw_Tests tp ruleTree =
    let ------------------------
        txName = show SwapOffer_Withdraw_Tx
        txSpecs = swapOffer_Withdraw_TxSpecs tp 100 100
        defaultTestCaseParams = generateTestCaseParams txSpecs
        selectedRedeemer = RedeemerLogValidator (Just SwapOffer_Withdraw_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp txSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer txSpecs ruleTree redeemerTestConfigTree
     in ------------------------

        transaction_Tests_Gen tp selectedRedeemer txName (P.const txSpecs) [] [] updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------

swapOffer_Validator_Redeemer_SwapFTxADA_Tests :: TestParams -> RuleTree -> Tasty.TestTree
swapOffer_Validator_Redeemer_SwapFTxADA_Tests tp ruleTree =
    let ------------------------
        txName = show SwapOffer_SwapFTxADA_Tx
        txSpecs = swapOffer_SwapFTxADA_TxSpecs tp
        ------------
        txParams_Default =
                [
                    TxParam "token_FT_Price1xe6" (1_000_000::Integer),
                    TxParam "amount_FT" (5::Integer),
                    TxParam "available_ADA" ((1_000 * 1_000_000_000)::Integer),
                    TxParam "invalidAmount" (5::Integer)

                ]
        ------------
        txParamsGenerators_Valid =
            TxParamGenerators
                [
                    intRangeParam "token_FT_Price1xe6" 1_000_000 1_000_000_000, -- FT Price 1xe6 -> rango real 1 a 1_000
                    intRangeParam "amount_FT" 1 10_000, -- FT amount
                    intRangeParam "available_ADA" (10_000 * 1_000) (2 * 1_000 * 1_000_000_000)  -- ADA available
                    -- me aseguro que el ada disponible sea mayor al valor de la conversion
                ]
        txParamsGenerators_InvalidAvailable =
            TxParamGenerators
                [
                    intRangeParam "token_FT_Price1xe6" 1_000_000_000 2_000_000_000, -- FT Price 1xe6 -> rango real 1_000 a 2_000
                    intRangeParam "amount_FT" 10_000 20_000, -- FT amount
                    intRangeParam "available_ADA" 0 (9_999 * 1_000) -- ADA available
                ]
        ------------
        txParamsGenerators_InvalidAmount =
           txParamsGenerators_Valid P.<> TxParamGenerators [intRangeParam "invalidAmount" 1 1_000] -- invalid amount to add a conversion or a commission
        ------------
        txParamsGenerators_List =
            [
                ("Valid FT Amount", txParamsGenerators_Valid),
                ("not isAmount_ADA_Available", txParamsGenerators_InvalidAvailable),
                ("Invalid_Conversion", txParamsGenerators_InvalidAmount),
                ("Invalid_Commissions", txParamsGenerators_InvalidAmount)
            ]
        ------------------------
        defaultTxSpecs = txSpecs txParams_Default
        defaultTestCaseParams = generateTestCaseParams defaultTxSpecs
        selectedRedeemer = RedeemerLogValidator (Just SwapOffer_SwapFTxADA_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp defaultTxSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer defaultTxSpecs ruleTree redeemerTestConfigTree
     in ------------------------

        transaction_Tests_Gen tp selectedRedeemer txName txSpecs txParams_Default txParamsGenerators_List updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------

swapOffer_Validator_Redeemer_SwapADAxFT_Tests :: TestParams -> RuleTree -> Tasty.TestTree
swapOffer_Validator_Redeemer_SwapADAxFT_Tests tp ruleTree =
    let ------------------------
        txName = show SwapOffer_SwapADAxFT_Tx
        txSpecs = swapOffer_SwapADAxFT_TxSpecs tp
        ------------
        txParams_Default =
                [
                    TxParam "token_FT_Price1xe6" (1_000_000::Integer),
                    TxParam "amount_ADA" (5_000_000::Integer),
                    TxParam "available_FT" ((1_000 * 1_000_000_000)::Integer),
                    TxParam "invalidAmount" (5::Integer)
                ]
        ------------
        txParamsGenerators_Valid =
            TxParamGenerators
                [
                    intRangeParam "token_FT_Price1xe6" 1_000_000 1_000_000_000, -- FT Price 1xe6 -> rango real 1 a 1_000
                    intRangeParam "amount_ADA" 1_000 10_000_000, -- ADA amount
                    intRangeParam "available_FT" (10_000_000 `divide` 1) (2 * 10_000_000 `divide` 1)-- FT available
                ]
        txParamsGenerators_InvalidAvailable =
            TxParamGenerators
                [
                    intRangeParam "token_FT_Price1xe6" 1_000_000_000 2_000_000_000, -- FT Price 1xe6 -> rango real 1_000 a 2_000
                    intRangeParam "amount_ADA" 10_000_000 20_000_000, -- ADA amount
                    intRangeParam "available_FT" 0 ((10_000_000 `divide` 2_000 )-1) -- FT available
                ]
        ------------
        txParamsGenerators_InvalidAmount =
           txParamsGenerators_Valid P.<> TxParamGenerators [intRangeParam "invalidAmount" 1 1_000] -- invalid amount to add a conversion or a commission
        ------------
        txParamsGenerators_List =
            [("Valid ADA Amount", txParamsGenerators_Valid), ("not isAmount_FT_Available", txParamsGenerators_InvalidAvailable), ("Invalid_Conversion", txParamsGenerators_InvalidAmount), ("Invalid_Commissions", txParamsGenerators_InvalidAmount)]
        ------------------------
        defaultTxSpecs = txSpecs txParams_Default
        defaultTestCaseParams = generateTestCaseParams defaultTxSpecs
        selectedRedeemer = RedeemerLogValidator (Just SwapOffer_SwapADAxFT_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp defaultTxSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer defaultTxSpecs ruleTree redeemerTestConfigTree
     in ------------------------

        transaction_Tests_Gen tp selectedRedeemer txName txSpecs txParams_Default txParamsGenerators_List updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------

swapOffer_Validator_Redeemer_Delete_Tests :: TestParams -> RuleTree -> Tasty.TestTree
swapOffer_Validator_Redeemer_Delete_Tests tp ruleTree =
    let ------------------------
        txName = show SwapOffer_Delete_Tx
        txSpecs = swapOffer_Delete_TxSpecs tp
        defaultTestCaseParams = generateTestCaseParams txSpecs
        selectedRedeemer = RedeemerLogValidator (Just SwapOffer_Delete_TestRedeemer)
        ------------------------
        redeemerTestConfigTree = getTestConfigTree tp txSpecs
        updatedTestConfigTree = updateConfigTreeFromRuleTree swTraceRuleTree txName selectedRedeemer txSpecs ruleTree redeemerTestConfigTree
     in ------------------------

        transaction_Tests_Gen tp selectedRedeemer txName (P.const txSpecs) [] [] updatedTestConfigTree defaultTestCaseParams

--------------------------------------------------------------------------------

swapOffer_Validator_Redeemer_Emergency_Tests :: TestParams -> RuleTree -> Tasty.TestTree
swapOffer_Validator_Redeemer_Emergency_Tests tp _ =
    let ------------------------
        txName = show SwapOffer_Emergency_Tx
        txSpecs = swapOffer_UpdateMinADA_TxSpecs tp
        ------------
        txParams_Default =
                [
                    TxParam "newMinADA" (toAlter_minAda::Integer),
                    TxParam "availableADA" (5_000_000::Integer)
                ]
        ------------
        defaultTxSpecs = txSpecs txParams_Default
        selectedRedeemer = RedeemerLogValidator (Just SwapOffer_Emergency_TestRedeemer)
        ------------------------
     in
        adminTokens_Tests_Gen tp txName selectedRedeemer (P.const defaultTxSpecs) SwapOfferT.mkEmergencyRedeemer (tpTokenAdminPolicy_CS tp) (tpTokenEmergencyAdminPolicy_CS tp) T.protocolTokenAdmin_TN T.protocolTokenEmergencyAdmin_TN False True

--------------------------------------------------------------------------------
