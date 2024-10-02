--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4
{- |
Module      : SellOffer.MintingPolicy
Description : Validation logic and tests related to the SellOffer minting policy.

This module defines the validation logic for the SellOffer's minting policy.

It includes multiple test cases to ensure the integrity and correctness of the
minting script.
-}
module Contracts.SellOffer.MintingPolicy where
--------------------------------------------------------------------------------
-- Non-IOG imports
import qualified Test.Tasty                              as Tasty
import qualified Test.Tasty.HUnit                        as Tasty

-- IOG imports
import           PlutusTx.Prelude

-- Project imports
import qualified Plutus.V2.Ledger.Api                    as LedgerApiV2
import qualified Protocol.Protocol.Types                 as ProtocolT
import qualified Protocol.SellOffer.Types                as SellOfferT
import           TestUtils.Contracts.InitialData
import           TestUtils.Contracts.TxContext.SellOffer
import           TestUtils.HelpersMAYZ
import           TestUtils.TestContext.Asserts
import           TestUtils.TestContext.Helpers
import           TestUtils.Types
import           TestUtils.TypesMAYZ
import TestUtils.Constants

--------------------------------------------------------------------------------

sellOffer_Policy_Tests :: TestParams -> Tasty.TestTree
sellOffer_Policy_Tests tp =
    Tasty.testGroup
        "SellOffer Policy Tests"
        [
            sellOffer_Policy_Redeemer_MintID_Tests tp,
            sellOffer_Policy_Redeemer_BurnID_Tests tp
        ]

--------------------------------------------------------------------------------

sellOffer_Policy_Redeemer_MintID_Tests :: TestParams -> Tasty.TestTree
sellOffer_Policy_Redeemer_MintID_Tests tp =
    let
        ------------------------
        txName = "SellOffer_Create_Tx"
        selectedRedeemer = RedeemerLogPolicy (Just SellOffer_MintID_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = sellOffer_Create_TxContext tp
                in
                    [
                        Tasty.testCase "Minting ID correctly must succeed" $ do
                        results <- testContextWrapper tp ctx
                        (Nothing, results)
                            `assertResultsContainAnyOf` []

                        , Tasty.testCase "SellOffer with total_FT_Earned different from 0 in datum must fail" $ do
                            let
                                wrongDatum = SellOfferT.mkDatum $
                                                (sellOffer_DatumType_MockData tp)
                                                    { SellOfferT.sodTotal_FT_Earned = 1
                                                    }
                                wrongUTxO =
                                    (sellOffer_UTxO_MockData tp)
                                        { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum wrongDatum
                                        }

                                ctx' = ctx
                                        |> setOutputs [wrongUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                    `assertResultsContainAnyOf` ["not isCorrect_Output_SellOffer_Datum"]
                        , Tasty.testCase "SellOffer with total_ADA_Earned different from 0 in datum must fail" $ do
                            let
                                wrongDatum = SellOfferT.mkDatum $
                                                (sellOffer_DatumType_MockData tp)
                                                    { SellOfferT.sodTotal_ADA_Earned = 1
                                                    }
                                wrongUTxO =
                                    (sellOffer_UTxO_MockData tp)
                                        { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum wrongDatum
                                        }
                                ctx' = ctx
                                        |> setOutputs [wrongUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                    `assertResultsContainAnyOf` ["not isCorrect_Output_SellOffer_Datum"]
                        , Tasty.testCase "SellOffer with wrong comissionsin in datum must fail" $ do
                            let
                                wrongDatum = SellOfferT.mkDatum $
                                                (sellOffer_DatumType_MockData tp)
                                                    { SellOfferT.sodAskedCommission_InBPx1e3 = ProtocolT.mmdMax ( ProtocolT.pdCommissionSellOffer_InBPx1e3 ( protocol_DatumType_MockData tp )) + sum_ANY_INVALID_NUMBER}
                                wrongUTxO =
                                    (sellOffer_UTxO_MockData tp)
                                        { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum wrongDatum
                                        }

                                ctx' = ctx
                                        |> setOutputs [wrongUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                    `assertResultsContainAnyOf` ["not isInRange commissionSellOffer_InBPx1e3"]
                        , Tasty.testCase "SellOffer with minimum allowed commission in datum must succeed" $ do
                            let
                                validDatum = SellOfferT.mkDatum $
                                                (sellOffer_DatumType_MockData tp)
                                                    { SellOfferT.sodAskedCommission_InBPx1e3 = ProtocolT.mmdMin (ProtocolT.pdCommissionSellOffer_InBPx1e3 (protocol_DatumType_MockData tp))
                                                    }
                                validUTxO =
                                    (sellOffer_UTxO_MockData tp)
                                        { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum validDatum
                                        }
                                ctx' = ctx
                                        |> setOutputs [validUTxO]
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                    `assertResultsContainAnyOf` []
                        , Tasty.testCase "SellOffer with maximum allowed commission in datum must succeed" $ do
                            let
                                validDatum = SellOfferT.mkDatum $
                                                (sellOffer_DatumType_MockData tp)
                                                    { SellOfferT.sodAskedCommission_InBPx1e3 = ProtocolT.mmdMax (ProtocolT.pdCommissionSellOffer_InBPx1e3 (protocol_DatumType_MockData tp))
                                                    }
                                validUTxO =
                                    (sellOffer_UTxO_MockData tp)
                                        { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum validDatum
                                        }
                                ctx' = ctx
                                        |> setOutputs [validUTxO]
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                    `assertResultsContainAnyOf` []
                        , Tasty.testCase "SellOffer with wrong MAYZ in datum must fail" $ do
                            let
                                wrongDatum = SellOfferT.mkDatum $
                                                (sellOffer_DatumType_MockData tp)
                                                    { SellOfferT.sodMAYZ =  ProtocolT.pdRequiredMAYZForSellOffer (protocol_DatumType_MockData tp )+ sum_ANY_INVALID_NUMBER}
                                wrongUTxO =
                                    (sellOffer_UTxO_MockData tp)
                                        { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum wrongDatum
                                        }

                                ctx' = ctx
                                        |> setOutputs [wrongUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                    `assertResultsContainAnyOf` ["not isCorrect_Output_SellOffer_Datum"]

                    ]


--------------------------------------------------------------------------------

sellOffer_Policy_Redeemer_BurnID_Tests :: TestParams -> Tasty.TestTree
sellOffer_Policy_Redeemer_BurnID_Tests tp =
    let
        ------------------------
        txName = "SellOffer_Delete_Tx"
        selectedRedeemer = RedeemerLogPolicy (Just SellOffer_BurnID_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = sellOffer_Delete_TxContext tp
                in
                    [
                        Tasty.testCase "Burning ID correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                    ]

--------------------------------------------------------------------------------
