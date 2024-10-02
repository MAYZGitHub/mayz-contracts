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
import qualified Test.Tasty                              as Tasty
import qualified Test.Tasty.HUnit                        as Tasty

-- IOG imports
import           PlutusTx.Prelude

-- Project imports
import qualified Generic.OnChainHelpers                  as OnChainHelpers
import qualified Protocol.Constants                      as T
import qualified Protocol.Protocol.Types                 as ProtocolT
import           TestUtils.Contracts.InitialData
import           TestUtils.Contracts.TxContext.SwapOffer
import           TestUtils.HelpersMAYZ
import           TestUtils.TestContext.Asserts
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------

swapOffer_Validator_Tests :: TestParams -> Tasty.TestTree
swapOffer_Validator_Tests tp =
    Tasty.testGroup
        "SwapOffer Validator Tests"
        [ swapOffer_Validator_Redeemer_UpdateStatus_Tests tp
        , swapOffer_Validator_Redeemer_UpdateAskedCommissionRate_Tests tp
        , swapOffer_Validator_Redeemer_UpdateSellRestrictions_Tests tp
        , swapOffer_Validator_Redeemer_UpdateMinADA_Tests tp
        , swapOffer_Validator_Redeemer_Deposit_Tests tp
        , swapOffer_Validator_Redeemer_Withdraw_Tests tp
        , swapOffer_Validator_Redeemer_SwapFTxADA_Tests tp
        , swapOffer_Validator_Redeemer_SwapADAxFT_Tests tp
        , swapOffer_Validator_Redeemer_Delete_Tests tp
        ]

--------------------------------------------------------------------------------

swapOffer_Validator_Redeemer_UpdateStatus_Tests :: TestParams -> Tasty.TestTree
swapOffer_Validator_Redeemer_UpdateStatus_Tests tp =
    let
        ------------------------
        txName = "SwapOffer_UpdateStatus_Tx"
        selectedRedeemer = RedeemerLogValidator (Just SwapOffer_UpdateStatus_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = swapOffer_UpdateStatus_TxContext tp T.swapOffer_Status_Open
                in
                    [
                        Tasty.testCase "Updating Datum correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                    ]

--------------------------------------------------------------------------------

swapOffer_Validator_Redeemer_UpdateAskedCommissionRate_Tests :: TestParams -> Tasty.TestTree
swapOffer_Validator_Redeemer_UpdateAskedCommissionRate_Tests tp =
    let
        ------------------------
        txName = "SwapOffer_UpdateAskedCommissionRate_Tx"
        selectedRedeemer = RedeemerLogValidator (Just SwapOffer_UpdateAskedCommissionRate_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = swapOffer_UpdateAskedCommissionRate_TxContext tp (ProtocolT.mmdMin $ ProtocolT.pdCommissionSwapOffer_InBPx1e3 $ protocol_DatumType_MockData tp)
                in
                    [
                        Tasty.testCase "Updating Asked Commission correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                    ]

--------------------------------------------------------------------------------

swapOffer_Validator_Redeemer_UpdateSellRestrictions_Tests :: TestParams -> Tasty.TestTree
swapOffer_Validator_Redeemer_UpdateSellRestrictions_Tests tp =
    let
        ------------------------
        txName = "SwapOffer_UpdateSellRestrictions_Tx"
        selectedRedeemer = RedeemerLogValidator (Just SwapOffer_UpdateSellRestrictions_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = swapOffer_UpdateSellRestrictions_TxContext tp T.swapOffer_NotAllowSell T.swapOffer_NotAllowSell
                in
                    [
                        Tasty.testCase "Updating Sell Restrictions correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                    ]

--------------------------------------------------------------------------------

swapOffer_Validator_Redeemer_UpdateMinADA_Tests :: TestParams -> Tasty.TestTree
swapOffer_Validator_Redeemer_UpdateMinADA_Tests tp =
    let
        ------------------------
        txName = "SwapOffer_UpdateMinADA_Tx"
        selectedRedeemer = RedeemerLogValidator (Just SwapOffer_UpdateMinADA_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = swapOffer_UpdateMinADA_TxContext tp toAlter_minAda
                in
                    [
                        Tasty.testCase "Changing min ADA correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                    ]

--------------------------------------------------------------------------------

swapOffer_Validator_Redeemer_Deposit_Tests :: TestParams -> Tasty.TestTree
swapOffer_Validator_Redeemer_Deposit_Tests tp =
    let
        ------------------------
        txName = "SwapOffer_Deposit_Tx"
        selectedRedeemer = RedeemerLogValidator (Just SwapOffer_Deposit_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = swapOffer_Deposit_TxContext tp 100 100
                in
                    [
                        Tasty.testCase "Depositing correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                    ]

--------------------------------------------------------------------------------

swapOffer_Validator_Redeemer_Withdraw_Tests :: TestParams -> Tasty.TestTree
swapOffer_Validator_Redeemer_Withdraw_Tests tp =
    let
        ------------------------
        txName = "SwapOffer_Withdraw_Tx"
        selectedRedeemer = RedeemerLogValidator (Just SwapOffer_Withdraw_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = swapOffer_Withdraw_TxContext tp 100 100
                in
                    [
                        Tasty.testCase "Withdrawing correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                    ]

--------------------------------------------------------------------------------

swapOffer_Validator_Redeemer_SwapFTxADA_Tests :: TestParams -> Tasty.TestTree
swapOffer_Validator_Redeemer_SwapFTxADA_Tests tp =
    let
        ------------------------
        txName = "SwapOffer_SwapFTxADA_Tx"
        selectedRedeemer = RedeemerLogValidator (Just SwapOffer_SwapFTxADA_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
        price_FT_in_ADA = 1_000_000
        amount_FT = 5
        commission = (ProtocolT.mmdDef $ ProtocolT.pdCommissionSwapOffer_InBPx1e3 $ protocol_DatumType_MockData tp)
        --------
        amount_ADA = OnChainHelpers.multiply_By_Scaled_1e6_And_RoundUp amount_FT price_FT_in_ADA
        commission_ADA = OnChainHelpers.multiply_By_Scaled_BPx1e3_And_RoundUp amount_ADA commission
        --------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = swapOffer_SwapFTxADA_TxContext tp commission price_FT_in_ADA amount_FT amount_ADA commission_ADA
                in
                    [
                        Tasty.testCase "Swapping FT for ADA correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                    ]

--------------------------------------------------------------------------------

swapOffer_Validator_Redeemer_SwapADAxFT_Tests :: TestParams -> Tasty.TestTree
swapOffer_Validator_Redeemer_SwapADAxFT_Tests tp =
    let
        ------------------------
        txName = "SwapOffer_SwapADAxFT_Tx"
        selectedRedeemer = RedeemerLogValidator (Just SwapOffer_SwapADAxFT_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
        price_FT_in_ADA = 1_000_000
        amount_ADA = 5_000_000
        commission = (ProtocolT.mmdDef $ ProtocolT.pdCommissionSwapOffer_InBPx1e3 $ protocol_DatumType_MockData tp)
        -----------------
        amount_FT = OnChainHelpers.divide_By_Scaled_1e6_And_RoundDownSafe amount_ADA price_FT_in_ADA
        amount_ADA_real = OnChainHelpers.multiply_By_Scaled_1e6_And_RoundUp amount_FT price_FT_in_ADA
        --------
        commission_FT = OnChainHelpers.multiply_By_Scaled_BPx1e3_And_RoundUp amount_FT commission
        -----------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = swapOffer_SwapADAxFT_TxContext tp commission price_FT_in_ADA amount_ADA_real amount_FT commission_FT
                in
                    [
                        Tasty.testCase "Swapping ADA for FT correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                    ]

--------------------------------------------------------------------------------

swapOffer_Validator_Redeemer_Delete_Tests :: TestParams -> Tasty.TestTree
swapOffer_Validator_Redeemer_Delete_Tests tp =
    let
        ------------------------
        txName = "SwapOffer_Delete_Tx"
        selectedRedeemer = RedeemerLogValidator (Just SwapOffer_Delete_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = swapOffer_Delete_TxContext tp
                in
                    [
                        Tasty.testCase "Deleting correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                    ]

--------------------------------------------------------------------------------
