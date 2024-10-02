--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4

{- |
Module      : Contracts.Fund.Validator
Description : Validation logic and tests related to the Fund module.

This module defines the validation logic for the Fund contracts, including the
creation and management of FundHoldings.

It includes multiple test cases to ensure the integrity and correctness of the
validation scripts.
-}
module Contracts.Fund.Validator where
--------------------------------------------------------------------------------4

-- Non-IOG imports
import           Prelude                            (show)
import qualified Test.Tasty                         as Tasty
import qualified Test.Tasty.HUnit                   as Tasty

-- IOG imports
import qualified Ledger.Ada                         as LedgerAda
import qualified Ledger.Address                     as LedgerAddress
import qualified Plutus.V2.Ledger.Api               as LedgerApiV2
import           PlutusTx.Prelude

-- Project imports
import qualified Generic.OffChainHelpers            as OffChainHelpers
import qualified Protocol.Constants                 as T
import qualified Protocol.Fund.Holding.Types        as FundHoldingT
import qualified Protocol.Fund.Types                as FundT
import           TestUtils.Contracts.InitialData
import           TestUtils.Contracts.TxContext.Fund
import           TestUtils.HelpersMAYZ
import           TestUtils.TestContext.Asserts
import           TestUtils.TestContext.Helpers
import           TestUtils.Types
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------

fund_Validator_Tests :: TestParams -> Tasty.TestTree
fund_Validator_Tests tp =
    Tasty.testGroup
        "Fund Validator Tests"
        [ fund_Validator_Redeemer_DatumUpdate_Tests tp
        , fund_Validator_Redeemer_UpdateMinADA_Tests tp
        , fund_Validator_Redeemer_FundHoldingAdd_Tests tp
        , fund_Validator_Redeemer_FundHoldingDelete_Tests tp
        , fund_Validator_Redeemer_Finish_Tests tp
        , fund_Validator_Redeemer_Delete_Tests tp
        ]

--------------------------------------------------------------------------------

fund_Validator_Redeemer_DatumUpdate_Tests :: TestParams -> Tasty.TestTree
fund_Validator_Redeemer_DatumUpdate_Tests tp =
    let
        ------------------------
        txName = show Fund_DatumUpdate_Tx
        selectedRedeemer = RedeemerLogValidator (Just Fund_DatumUpdate_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = fund_DatumUpdate_TxContext tp [] "aaff"
                in
                    [
                        Tasty.testCase "Update Datum with not change must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                        , Tasty.testCase "Emptying the list of admins must succeed" $ do
                            let modifiedDatumType = (fund_DatumType_MockData tp) {FundT.fdAdmins = []}
                                modifiedUTxO =
                                    (fund_UTxO_MockData tp)
                                        { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ FundT.mkDatum modifiedDatumType
                                        }
                                ctx' = ctx
                                        |> setOutputs [modifiedUTxO]
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                        , Tasty.testCase "Changing FundPolicyID must fail" $ do
                            let modifiedDatumType =
                                    (fund_DatumType_MockData tp)
                                        { FundT.fdFundPolicy_CS =
                                            "a4b8ef314629ab3c5012d3e58d6b4f2a3e9f5d47c2ad34e7f1e8c2fa"
                                        }
                                modifiedUTxO =
                                    (fund_UTxO_MockData tp)
                                        { LedgerApiV2.txOutDatum =
                                            LedgerApiV2.OutputDatum $
                                                FundT.mkDatum modifiedDatumType
                                        }
                                ctx' = ctx
                                        |> setOutputs [modifiedUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                `assertResultsContainAnyOf` ["not isCorrect_Output_Fund_Datum_Updated"]
                        , Tasty.testCase "Changing Fund UTxO value must fail" $ do
                            let modifiedUTxO =
                                    (fund_UTxO_MockData tp)
                                        { LedgerApiV2.txOutValue =
                                            toAlter_Value_Adding_SomeADA
                                                <> LedgerApiV2.txOutValue (fund_UTxO_MockData tp)
                                        }
                                ctx' = ctx
                                        |> setOutputs [modifiedUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                `assertResultsContainAnyOf` ["not isCorrect_Output_Fund_Value_NotChanged"]
                        ]

--------------------------------------------------------------------------------

fund_Validator_Redeemer_UpdateMinADA_Tests :: TestParams -> Tasty.TestTree
fund_Validator_Redeemer_UpdateMinADA_Tests tp =
    let ------------------------
        txName = show Fund_UpdateMinADA_Tx
        selectedRedeemer = RedeemerLogValidator (Just Fund_UpdateMinADA_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = fund_UpdateMinADA_TxContext tp toAlter_minAda
                in
                    [
                        Tasty.testCase "Changing min ADA correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                    ]

--------------------------------------------------------------------------------

fund_Validator_Redeemer_FundHoldingAdd_Tests :: TestParams -> Tasty.TestTree
fund_Validator_Redeemer_FundHoldingAdd_Tests tp =
    let
        ------------------------
        txName = show FundHolding_Create_Tx
        selectedRedeemer = RedeemerLogValidator (Just Fund_FundHoldingAdd_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = fund_FundHoldingAdd_TxContext tp
                in
                    [
                        Tasty.testCase "Add FundHolding correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                        , Tasty.testCase "Not minting FundHolding ID must fail" $ do
                            let ctx' = ctx
                                        |> setMintAndAddRedeemers mempty
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                `assertResultsContainAnyOf` ["not isMintingFundHoldingID"]
                        , Tasty.testCase "Changing Fund UTxO value must fail" $ do
                            let modifiedUTxO =
                                    (fund_UTxO_With_Added_FundHolding_MockData tp)
                                        { LedgerApiV2.txOutValue =
                                            toAlter_Value_Adding_SomeADA
                                                <> LedgerApiV2.txOutValue (fund_UTxO_With_Added_FundHolding_MockData tp)
                                        }
                                ctx' = ctx
                                        |> setOutputs [modifiedUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                `assertResultsContainAnyOf` ["not isCorrect_Output_Fund_Value_NotChanged"]
                        , Tasty.testCase "Not signed by admin must fail" $ do
                            let ctx' = ctx
                                        |> setSignatories []
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                `assertResultsContainAnyOf` ["not isSignedByAny admins nor isAdminTokenPresent"]
                        , Tasty.testCase "Not incrementing count must fail" $ do
                            let modifiedDatumType = (fund_DatumType_With_Added_FundHolding_MockData tp) {FundT.fdHoldingsCount = 0}
                                modifiedUTxO =
                                    (fund_UTxO_With_Added_FundHolding_MockData tp)
                                        { LedgerApiV2.txOutDatum =
                                            LedgerApiV2.OutputDatum $ FundT.mkDatum modifiedDatumType
                                        }
                                ctx' = ctx
                                        |> setOutputs [modifiedUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                `assertResultsContainAnyOf` ["not isCorrect_Output_Fund_Datum_With_HoldingAdded"]
                        , Tasty.testCase "Not incrementing index must fail" $ do
                            let modifiedDatumType = (fund_DatumType_With_Added_FundHolding_MockData tp) {FundT.fdHoldingsIndex = 0}
                                modifiedUTxO =
                                    (fund_UTxO_With_Added_FundHolding_MockData tp)
                                        { LedgerApiV2.txOutDatum =
                                            LedgerApiV2.OutputDatum $ FundT.mkDatum modifiedDatumType
                                        }
                                ctx' = ctx
                                        |> setOutputs [modifiedUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                `assertResultsContainAnyOf` ["not isCorrect_Output_Fund_Datum_With_HoldingAdded"]
                        ]

--------------------------------------------------------------------------------

fund_Validator_Redeemer_FundHoldingDelete_Tests :: TestParams -> Tasty.TestTree
fund_Validator_Redeemer_FundHoldingDelete_Tests tp =
    let
        ------------------------
        txName = show FundHolding_Delete_Tx
        selectedRedeemer = RedeemerLogValidator (Just Fund_FundHoldingDelete_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = fund_FundHoldingDelete_TxContext tp
                in
                    [
                        Tasty.testCase "Delete FundHolding correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                        , Tasty.testCase "Not burning FundHolding ID must fail" $ do
                            let ctx' = ctx
                                        |> setMintAndAddRedeemers
                                            []
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                `assertResultsContainAnyOf` ["not isBurningFundHoldingID"]
                        , Tasty.testCase "Minting another FundHolding ID must fail" $ do
                            let ctx' = ctx
                                        |> setMintAndAddRedeemers
                                            [
                                                ( LedgerApiV2.singleton
                                                    (FundT.fdFundHoldingPolicyID_CS (fund_DatumType_MockData tp))
                                                    (mkFundHoldingID_TN 0)
                                                    1
                                                , FundHoldingT.mkBurnIDRedeemer
                                                )
                                            ]
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                `assertResultsContainAnyOf` ["not isBurningFundHoldingID"]
                        , Tasty.testCase "Not decreasing holdingsCount must fail" $ do
                            let modifiedDatumType =
                                    (fund_DatumType_With_Deleted_FundHolding_MockData tp)
                                        { FundT.fdHoldingsCount =
                                            FundT.fdHoldingsCount (fund_DatumType_With_Added_FundHolding_MockData tp)
                                        }
                                modifiedUTxO =
                                    (fund_UTxO_With_Deleted_FundHolding_MockData tp)
                                        { LedgerApiV2.txOutDatum =
                                            LedgerApiV2.OutputDatum $ FundT.mkDatum modifiedDatumType
                                        }
                                ctx' = ctx
                                        |> setOutputs [modifiedUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                `assertResultsContainAnyOf` ["not isCorrect_Output_Fund_Datum_With_HoldingDeleted"]
                        , Tasty.testCase "Increasing holdingsCount must fail" $ do
                            let modifiedDatumType =
                                    (fund_DatumType_With_Deleted_FundHolding_MockData tp)
                                        { FundT.fdHoldingsCount =
                                            FundT.fdHoldingsCount (fund_DatumType_With_Deleted_FundHolding_MockData tp) + 1
                                        }
                                modifiedUTxO =
                                    (fund_UTxO_With_Deleted_FundHolding_MockData tp)
                                        { LedgerApiV2.txOutDatum =
                                            LedgerApiV2.OutputDatum $ FundT.mkDatum modifiedDatumType
                                        }
                                ctx' = ctx
                                        |> setOutputs [modifiedUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                `assertResultsContainAnyOf` ["not isCorrect_Output_Fund_Datum_With_HoldingDeleted"]
                        , Tasty.testCase "Changing Fund UTxO value must fail" $ do
                            let modifiedUTxO =
                                    (fund_UTxO_With_Deleted_FundHolding_MockData tp)
                                        { LedgerApiV2.txOutValue =
                                            toAlter_Value_Adding_SomeADA
                                                <> LedgerApiV2.txOutValue (fund_UTxO_With_Deleted_FundHolding_MockData tp)
                                        }
                                ctx' = ctx
                                        |> setOutputs [modifiedUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                `assertResultsContainAnyOf` ["not isCorrect_Output_Fund_Value_NotChanged"]
                        ]

--------------------------------------------------------------------------------

fund_Validator_Redeemer_Finish_Tests :: TestParams -> Tasty.TestTree
fund_Validator_Redeemer_Finish_Tests tp =
    let ------------------------
        txName = show Fund_Finish_Tx
        selectedRedeemer = RedeemerLogValidator (Just Fund_Finish_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = fund_Finish_TxContext tp (tpTransactionDate tp)
                in
                    [
                        Tasty.testCase "Finish Fund correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                    ]

--------------------------------------------------------------------------------

fund_Validator_Redeemer_Delete_Tests :: TestParams -> Tasty.TestTree
fund_Validator_Redeemer_Delete_Tests tp =
    let
        ------------------------
        txName = show Fund_Delete_Tx
        selectedRedeemer = RedeemerLogValidator (Just Fund_Delete_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = fund_Delete_TxContext tp
                in
                    [
                        Tasty.testCase "Delete correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                        , Tasty.testCase "Not burning FundID must fail" $ do
                            let ctx' = ctx
                                        |> setMintAndAddRedeemers
                                            []
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                `assertResultsContainAnyOf` ["not isBurningFundID"]
                        , Tasty.testCase "Minting another FundID must fail" $ do
                            let ctx' = ctx
                                        |> setMintAndAddRedeemers
                                            [
                                                ( LedgerApiV2.singleton
                                                    (FundT.fdFundPolicy_CS (fund_DatumType_MockData tp))
                                                    T.fundID_TN
                                                    (negate 1)
                                                    <> LedgerApiV2.singleton
                                                        (FundT.fdFundPolicy_CS (fund_DatumType_MockData tp))
                                                        T.investUnitID_TN
                                                        (negate 1)
                                                    <> LedgerApiV2.singleton
                                                        (FundT.fdFundPolicy_CS (fund_DatumType_MockData tp))
                                                        T.fundID_TN
                                                        1
                                                , FundT.mkBurnIDRedeemer
                                                )
                                            ]
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                `assertResultsContainAnyOf` ["not isBurningFundID"]
                        , -- Double satisfaction: try to steal the Protocol ID by replacing
                        -- it with another token.
                        Tasty.testCase "Double satisfaction must fail" $ do
                            let fundPolicy_CS' = "00000000000000000000000000000000000000000000000000000000"
                                fund_DatumType' =
                                    (OffChainHelpers.getUnsafe_DatumType_From_TxOutOutputDatum (fund_UTxO_MockData tp) FundT.getFund_DatumType)
                                        { FundT.fdFundPolicy_CS = fundPolicy_CS'
                                        }
                                fund_UTxO_MockData' =
                                    LedgerApiV2.TxOut
                                        (OffChainHelpers.addressValidator (tpFundValidator_Hash tp))
                                        (LedgerAda.lovelaceValueOf minAdaFundDatum <> LedgerApiV2.singleton fundPolicy_CS' T.fundID_TN 1)
                                        (LedgerApiV2.OutputDatum $ FundT.mkDatum fund_DatumType')
                                        Nothing
                                walletUTxO =
                                    LedgerApiV2.TxOut
                                        (LedgerAddress.pubKeyHashAddress (LedgerAddress.PaymentPubKeyHash "a2") Nothing)
                                        (LedgerAda.lovelaceValueOf minAdaForUTxOWithTokens <> LedgerApiV2.singleton fundPolicy_CS' T.fundID_TN 1)
                                        LedgerApiV2.NoOutputDatum
                                        Nothing
                                ctx' = ctx
                                        |> setInputsAndAddRedeemers
                                            [(fund_UTxO_MockData tp, FundT.mkDeleteRedeemer), (fund_UTxO_MockData', FundT.mkDeleteRedeemer)]
                                        |> setOutputs
                                            [walletUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                `assertResultsContainAnyOf` ["Expected exactly one Fund input"]
                        ]

--------------------------------------------------------------------------------
