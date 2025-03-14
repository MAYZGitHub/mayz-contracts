--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4
{- |
Module      : SwapOffer.MintingPolicy
Description : Validation logic and tests related to the SwapOffer minting policy.

This module defines the validation logic for the SwapOffer's minting policy.

It includes multiple test cases to ensure the integrity and correctness of the
minting script.
-}
module Contracts.SwapOffer.MintingPolicy where
--------------------------------------------------------------------------------
-- Non-IOG imports
import qualified Test.Tasty                              as Tasty
import qualified Test.Tasty.HUnit                        as Tasty

-- IOG imports
import           PlutusTx.Prelude

-- Project imports
import qualified Plutus.V2.Ledger.Api                    as LedgerApiV2
import qualified Protocol.Protocol.Types                 as ProtocolT
import qualified Protocol.SwapOffer.Types                as SwapOfferT
import           TestUtils.Constants
import           TestUtils.Contracts.InitialData
import           TestUtils.Contracts.TxContext.SwapOffer
import           TestUtils.HelpersMAYZ
import           TestUtils.TestContext.Asserts
import           TestUtils.TestContext.Helpers
import           TestUtils.Types
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------

swapOffer_Policy_Tests :: TestParams -> Tasty.TestTree
swapOffer_Policy_Tests tp =
    Tasty.testGroup
        "SwapOffer Policy Tests"
        [
            swapOffer_Policy_Redeemer_MintID_Tests tp,
            swapOffer_Policy_Redeemer_BurnID_Tests tp
        ]

--------------------------------------------------------------------------------

swapOffer_Policy_Redeemer_MintID_Tests :: TestParams -> Tasty.TestTree
swapOffer_Policy_Redeemer_MintID_Tests tp =
    let
        ------------------------
        txName = "SwapOffer_Create_Tx"
        selectedRedeemer = RedeemerLogPolicy (Just SwapOffer_MintID_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = swapOffer_Create_TxContext tp
                in
                    [
                        Tasty.testCase "Minting ID correctly must succeed" $ do
                        results <- testContextWrapper tp ctx
                        (Nothing, results)
                            `assertResultsContainAnyOf` []

                        , Tasty.testCase "SwapOffer with total_FT_Earned different from 0 in datum must fail" $ do
                            let
                                wrongDatum = SwapOfferT.mkDatum $
                                                (swapOffer_DatumType_MockData tp)
                                                    { SwapOfferT.sodTotal_FT_Earned = 1
                                                    }
                                wrongUTxO =
                                    (swapOffer_UTxO_MockData tp)
                                        { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum wrongDatum
                                        }

                                ctx' = ctx
                                        |> setOutputs [wrongUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                    `assertResultsContainAnyOf` ["not isCorrect_Output_SwapOffer_Datum"]
                        , Tasty.testCase "SwapOffer with total_ADA_Earned different from 0 in datum must fail" $ do
                            let
                                wrongDatum = SwapOfferT.mkDatum $
                                                (swapOffer_DatumType_MockData tp)
                                                    { SwapOfferT.sodTotal_ADA_Earned = 1
                                                    }
                                wrongUTxO =
                                    (swapOffer_UTxO_MockData tp)
                                        { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum wrongDatum
                                        }
                                ctx' = ctx
                                        |> setOutputs [wrongUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                    `assertResultsContainAnyOf` ["not isCorrect_Output_SwapOffer_Datum"]
                        , Tasty.testCase "SwapOffer with wrong comissionsin in datum must fail" $ do
                            let
                                wrongDatum = SwapOfferT.mkDatum $
                                                (swapOffer_DatumType_MockData tp)
                                                    { SwapOfferT.sodAskedCommission_InBPx1e3 = ProtocolT.mmdMax ( ProtocolT.pdCommissionSwapOffer_InBPx1e3 ( protocol_DatumType_MockData tp )) + sum_ANY_INVALID_NUMBER}
                                wrongUTxO =
                                    (swapOffer_UTxO_MockData tp)
                                        { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum wrongDatum
                                        }

                                ctx' = ctx
                                        |> setOutputs [wrongUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                    `assertResultsContainAnyOf` ["not isInRange commissionSwapOffer_InBPx1e3"]
                        , Tasty.testCase "SwapOffer with minimum allowed commission in datum must succeed" $ do
                            let
                                validDatum = SwapOfferT.mkDatum $
                                                (swapOffer_DatumType_MockData tp)
                                                    { SwapOfferT.sodAskedCommission_InBPx1e3 = ProtocolT.mmdMin (ProtocolT.pdCommissionSwapOffer_InBPx1e3 (protocol_DatumType_MockData tp))
                                                    }
                                validUTxO =
                                    (swapOffer_UTxO_MockData tp)
                                        { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum validDatum
                                        }
                                ctx' = ctx
                                        |> setOutputs [validUTxO]
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                    `assertResultsContainAnyOf` []
                        , Tasty.testCase "SwapOffer with maximum allowed commission in datum must succeed" $ do
                            let
                                validDatum = SwapOfferT.mkDatum $
                                                (swapOffer_DatumType_MockData tp)
                                                    { SwapOfferT.sodAskedCommission_InBPx1e3 = ProtocolT.mmdMax (ProtocolT.pdCommissionSwapOffer_InBPx1e3 (protocol_DatumType_MockData tp))
                                                    }
                                validUTxO =
                                    (swapOffer_UTxO_MockData tp)
                                        { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum validDatum
                                        }
                                ctx' = ctx
                                        |> setOutputs [validUTxO]
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                    `assertResultsContainAnyOf` []
                        , Tasty.testCase "SwapOffer with wrong MAYZ in datum must fail" $ do
                            let
                                wrongDatum = SwapOfferT.mkDatum $
                                                (swapOffer_DatumType_MockData tp)
                                                    { SwapOfferT.sodRequiredMAYZ =  ProtocolT.pdRequiredMAYZForSwapOffer (protocol_DatumType_MockData tp )+ sum_ANY_INVALID_NUMBER}
                                wrongUTxO =
                                    (swapOffer_UTxO_MockData tp)
                                        { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum wrongDatum
                                        }

                                ctx' = ctx
                                        |> setOutputs [wrongUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                    `assertResultsContainAnyOf` ["not isCorrect_Output_SwapOffer_Datum"]

                    ]


--------------------------------------------------------------------------------

swapOffer_Policy_Redeemer_BurnID_Tests :: TestParams -> Tasty.TestTree
swapOffer_Policy_Redeemer_BurnID_Tests tp =
    let
        ------------------------
        txName = "SwapOffer_Delete_Tx"
        selectedRedeemer = RedeemerLogPolicy (Just SwapOffer_BurnID_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = swapOffer_Delete_TxContext tp
                in
                    [
                        Tasty.testCase "Burning ID correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                    ]

--------------------------------------------------------------------------------
