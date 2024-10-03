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

--------------------------------------------------------------------------------4

-- Non-IOG imports
import qualified GHC.Stack as GHC
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Tasty
import Prelude as P

-- IOG imports
import qualified Ledger
import qualified Ledger.Ada as LedgerAda
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import PlutusTx.Prelude (divide)

-- Project imports

import qualified Generic.Constants as T
import qualified Protocol.Fund.Helpers as FundHelpers
import qualified Protocol.Fund.Holding.Types as FundHoldingT
import qualified Protocol.Fund.Types as FundT
import qualified Protocol.InvestUnit.Types as InvestUnitT
import qualified Protocol.OnChainHelpers as OnChainHelpers
import qualified Protocol.PABTypes as T
import qualified Protocol.Types as T
import TestUtils.Contracts.InitialData
import TestUtils.Contracts.TxContext.FundHolding
import TestUtils.HelpersMAYZ
import TestUtils.TestContext.Asserts
import TestUtils.TestContext.Helpers
import TestUtils.Types
import TestUtils.TypesMAYZ

--------------------------------------------------------------------------------

fundHolding_Validator_Tests :: TestParams -> Tasty.TestTree
fundHolding_Validator_Tests tp =
    Tasty.testGroup
        "FundHolding Validator Tests"
        [ fundHolding_Validator_Redeemer_UpdateMinADA_Tests tp
        , fundHolding_Validator_Redeemer_Deposit_Tests tp
        , fundHolding_Validator_Redeemer_Withdraw_Tests tp
        , fundHolding_Validator_Redeemer_Collect_Protocol_Commission_Tests tp
        , fundHolding_Validator_Redeemer_Collect_Managers_Commission_Tests tp
        , fundHolding_Validator_Redeemer_Collect_Delegators_Commission_Tests tp
        , fundHolding_Validator_Redeemer_ReIndexing_Tests tp
        , fundHolding_Validator_Redeemer_BalanceAssets_Tests tp
        , fundHolding_Validator_Redeemer_Delete_Tests tp
        ]

--------------------------------------------------------------------------------

fundHolding_Validator_Redeemer_UpdateMinADA_Tests :: TestParams -> Tasty.TestTree
fundHolding_Validator_Redeemer_UpdateMinADA_Tests tp =
    let
        ------------------------
        txName = show FundHolding_UpdateMinADA_Tx
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_UpdateMinADA_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
    in
        ------------------------

        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
            let
                ctx = fundHolding_UpdateMinADA_TxContext tp toAlter_minAda
            in
                [ Tasty.testCase "Changing min ADA correctly must succeed" $ do
                    let
                        ctx' = ctx
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                ]

--------------------------------------------------------------------------------

fundHolding_Validator_Redeemer_Deposit_Tests :: TestParams -> Tasty.TestTree
fundHolding_Validator_Redeemer_Deposit_Tests tp =
    let
        ------------------------
        txName = show Fund_Deposit_Tx
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_Deposit_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
    in
        ------------------------

        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
            let
                ctx = fundHolding_Deposit_TxContext tp (tpDepositDate tp) deposit_MockData
            in
                [ Tasty.testCase "Depositing correctly must succeed" $ do
                    let
                        ctx' = ctx
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                , Tasty.testCase "Depositing without minting FT must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setMintAndAddRedeemers mempty
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isMintingFT"]
                , Tasty.testCase "Depositing without increasing FT minted subtotal must fail" $ do
                    let
                        modifiedDatumType =
                            (fundHolding_DatumType_With_Deposits_MockData tp)
                                { FundHoldingT.hdSubtotal_FT_Minted =
                                    FundHoldingT.hdSubtotal_FT_Minted (fundHolding_DatumType_With_NoDeposits_MockData tp)
                                }
                        modifiedUTxO =
                            (fundHolding_UTxO_With_Deposits_MockData tp)
                                { LedgerApiV2.txOutDatum =
                                    LedgerApiV2.OutputDatum $ FundHoldingT.mkDatum modifiedDatumType
                                }
                        ctx' =
                            ctx
                                |> setOutputs [modifiedUTxO]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isCorrect_Output_FundHolding_Datum_With_Deposit"]
                , Tasty.testCase "Depositing without paying invest units must fail" $ do
                    let
                        (_, commissionsFT_MockData, _) = calculateDepositCommissionsUsingMonths_ tp (tpDepositDate tp) deposit_MockData
                        modifiedUTxO =
                            (fundHolding_UTxO_With_Deposits_MockData tp)
                                { LedgerApiV2.txOutValue =
                                    LedgerAda.lovelaceValueOf minAdaFundHoldingDatum
                                        <> LedgerApiV2.singleton (tpFundHoldingPolicyID_CS tp) (mkFundHoldingID_TN 0) 1
                                        <> LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) commissionsFT_MockData
                                }
                        ctx' =
                            ctx
                                |> setOutputs [modifiedUTxO]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isCorrect_Output_FundHolding_Value_With_Tokens_And_FT"]
                , Tasty.testCase "Depositing date outside valid range must fail" $ do
                    let
                        -- valid range for tx is created with tpDepositDate as date, and then subs and adds hald of valid time
                        -- (date'  - LedgerApiV2.POSIXTime (LedgerApiV2.getPOSIXTime T.validTimeRange `divide` 2) + 1) (date' + LedgerApiV2.POSIXTime (LedgerApiV2.getPOSIXTime T.validTimeRange `divide` 2) -1)
                        -- so if we set in redeemer the date (tpDepositDate tp+T.validTimeRange ) it must fail
                        ctx' =
                            ctx
                                |> setInputsAndAddRedeemers [(fundHolding_UTxO_With_NoDeposits_MockData tp, FundHoldingT.mkDepositRedeemer (tpDepositDate tp + T.validTimeRange) deposit_MockData)]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isDateInRange"]
                , Tasty.testCase "Depositing with invalid range must fail" $ do
                    let
                        ctx' = ctx |> setValidyRange (createInValidRange (tpDepositDate tp))
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isValidRange"]
                ]

--------------------------------------------------------------------------------

fundHolding_Validator_Redeemer_Withdraw_Tests :: GHC.HasCallStack => TestParams -> Tasty.TestTree
fundHolding_Validator_Redeemer_Withdraw_Tests tp =
    let
        ------------------------
        txName = show Fund_Withdraw_Tx
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_Withdraw_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
    in
        ------------------------

        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
            let
                ctx = fundHolding_Withdraw_TxContext tp (tpDepositDate tp) deposit_MockData (tpWithdrawDate tp) withdraw_MockData
            in
                [ Tasty.testCase "Withdrawing correctly must succeed" $ do
                    let
                        ctx' = ctx
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                , Tasty.testCase "Withdrawing without burning FT must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setMintAndAddRedeemers mempty
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isBurningFT"]
                , Tasty.testCase "Withdrawing without updating FT minted subtotal must fail" $ do
                    let
                        modifiedDatumType =
                            (fundHolding_DatumType_With_Withdraw_MockData tp)
                                { FundHoldingT.hdSubtotal_FT_Minted =
                                    FundHoldingT.hdSubtotal_FT_Minted (fundHolding_DatumType_With_Deposits_MockData tp)
                                }
                        modifiedUTxO =
                            (fundHolding_UTxO_With_Withdraw_MockData tp)
                                { LedgerApiV2.txOutDatum =
                                    LedgerApiV2.OutputDatum $ FundHoldingT.mkDatum modifiedDatumType
                                }
                        ctx' =
                            ctx
                                |> setOutputs [modifiedUTxO]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isCorrect_Output_FundHolding_Datum_With_Withdraw"]
                , Tasty.testCase "Withdrawing without user recovering commissions must fail" $ do
                    let
                        !investUnit_Granularity = OnChainHelpers.getDecimalsInInvestUnit (T.iuValues investUnit_Initial)
                        (_, withdrawPlusCommissionsGetBack_MockData, _) = calculateWithdrawCommissionsUsingMonths_ tp (tpWithdrawDate tp) withdraw_MockData investUnit_Granularity
                        modifiedUTxO =
                            (fundHolding_UTxO_With_Withdraw_MockData tp)
                                { LedgerApiV2.txOutValue =
                                    LedgerApiV2.txOutValue (fundHolding_UTxO_With_Deposits_MockData tp)
                                        -- <> LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) (-commissionsFTToGetBack_MockData)
                                        <> LedgerApiV2.singleton investUnit_Initial_Token_CS investUnit_Initial_Token_TN (-((withdrawPlusCommissionsGetBack_MockData * investUnit_Initial_Token_Amount) `divide` 100))
                                }
                        ctx' =
                            ctx
                                |> setOutputs [modifiedUTxO]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isCorrect_Output_FundHolding_Value_Without_Tokens_And_FT_for_Commissions"]
                ]

--------------------------------------------------------------------------------

fundHolding_Validator_Redeemer_Collect_Protocol_Commission_Tests :: TestParams -> Tasty.TestTree
fundHolding_Validator_Redeemer_Collect_Protocol_Commission_Tests tp =
    let
        ------------------------
        txName = show FundHolding_Collect_Protocol_Commission_Tx
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_Collect_Protocol_Commission_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
    in
        ------------------------

        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
            let
                ctx = fundHolding_Collect_Protocol_Commission_TxContext tp
            in
                [ Tasty.testCase "Collecting commissions correctly must succeed" $ do
                    let
                        ctx' = ctx
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                , Tasty.testCase "Collecting commissions without updating remaining commissions must fail" $ do
                    let
                        modifiedDatumType =
                            (fundHolding_DatumType_With_Deposits_MockData tp)
                                { FundHoldingT.hdSubtotal_FT_Commissions =
                                    FundHoldingT.hdSubtotal_FT_Commissions (fundHolding_DatumType_With_Deposits_MockData tp)
                                }
                        modifiedUTxO =
                            (fundHolding_UTxO_With_Collected_Protocol tp withdraw_Commissions_MockData)
                                { LedgerApiV2.txOutDatum =
                                    LedgerApiV2.OutputDatum $ FundHoldingT.mkDatum modifiedDatumType
                                }
                        ctx' =
                            ctx
                                |> setOutputs [modifiedUTxO]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isCorrect_Output_FundHolding_Datum_With_Collect_Protocol_Commission"]
                , Tasty.testCase "Collecting commissions without updating FundHolding value must fail" $ do
                    let
                        modifiedUTxO =
                            (fundHolding_UTxO_With_Collected_Protocol tp withdraw_Commissions_MockData)
                                { LedgerApiV2.txOutValue =
                                    LedgerApiV2.txOutValue (fundHolding_UTxO_With_Deposits_MockData tp)
                                }
                        ctx' =
                            ctx
                                |> setOutputs [modifiedUTxO]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isCorrect_Output_FundHolding_Value_Without_FT_for_Commissions"]
                , Tasty.testCase "Trying to collect more commissions than the available must fail" $ do
                    let
                        available = FundHelpers.getCommissionsAvailable (tpDeadline tp) (fundHolding_DatumType_With_Deposits_MockData tp) (tpShare_InBPx1e2_Protocol tp) (FundHoldingT.hdSubtotal_FT_Commissions_Collected_Protocol (fundHolding_DatumType_With_Deposits_MockData tp)) (tpCollectCommissionsDate tp)
                        withdraw_Commissions_MockData' = available + 1
                        ctx' =
                            ctx
                                |> setInputsAndAddRedeemers [(fundHolding_UTxO_With_Deposits_MockData tp, FundHoldingT.mkCollect_Protocol_CommissionRedeemer (tpCollectCommissionsDate tp) withdraw_Commissions_MockData')]
                                |> setOutputs [fundHolding_UTxO_With_Collected_Protocol tp withdraw_Commissions_MockData']
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isCommissionsAvailable"]
                , Tasty.testCase "Trying to collect commissions without adding admin signatory must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setSignatories []
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isSignedByAny admins nor isAdminTokenPresent"]
                , Tasty.testCase "Collecting commissions outside valid range must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setValidyRange (Ledger.interval 0 300_000)
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isDateInRange"]
                ]

fundHolding_Validator_Redeemer_Collect_Managers_Commission_Tests :: TestParams -> Tasty.TestTree
fundHolding_Validator_Redeemer_Collect_Managers_Commission_Tests tp =
    let
        ------------------------
        txName = show FundHolding_Collect_Managers_Commission_Tx
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_Collect_Managers_Commission_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
    in
        ------------------------

        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
            let
                ctx = fundHolding_Collect_Managers_Commission_TxContext tp
            in
                [ Tasty.testCase "Collecting commissions correctly must succeed" $ do
                    let
                        ctx' = ctx
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                , Tasty.testCase "Collecting commissions without updating remaining commissions must fail" $ do
                    let
                        modifiedDatumType =
                            (fundHolding_DatumType_With_Deposits_MockData tp)
                                { FundHoldingT.hdSubtotal_FT_Commissions =
                                    FundHoldingT.hdSubtotal_FT_Commissions (fundHolding_DatumType_With_Deposits_MockData tp)
                                }
                        modifiedUTxO =
                            (fundHolding_UTxO_With_Collected_Managers tp withdraw_Commissions_MockData)
                                { LedgerApiV2.txOutDatum =
                                    LedgerApiV2.OutputDatum $ FundHoldingT.mkDatum modifiedDatumType
                                }
                        ctx' =
                            ctx
                                |> setOutputs [modifiedUTxO]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isCorrect_Output_FundHolding_Datum_With_Collect_Managers_Commission"]
                , Tasty.testCase "Collecting commissions without updating FundHolding value must fail" $ do
                    let
                        modifiedUTxO =
                            (fundHolding_UTxO_With_Collected_Managers tp withdraw_Commissions_MockData)
                                { LedgerApiV2.txOutValue =
                                    LedgerAda.lovelaceValueOf minAdaFundHoldingDatum
                                        <> LedgerApiV2.singleton T.exampleCS T.exampleTN 300
                                        <> LedgerApiV2.singleton (tpFundHoldingPolicyID_CS tp) (mkFundHoldingID_TN 0) 1
                                        <> LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) 200
                                }
                        ctx' =
                            ctx
                                |> setOutputs [modifiedUTxO]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isCorrect_Output_FundHolding_Value_Without_FT_for_Commissions"]
                , Tasty.testCase "Trying to collect more commissions than the available must fail" $ do
                    let
                        available = FundHelpers.getCommissionsAvailable (tpDeadline tp) (fundHolding_DatumType_With_Deposits_MockData tp) (tpShare_InBPx1e2_Managers tp) (FundHoldingT.hdSubtotal_FT_Commissions_Collected_Managers (fundHolding_DatumType_With_Deposits_MockData tp)) (tpCollectCommissionsDate tp)
                        withdraw_Commissions_MockData' = available + 1
                        ctx' =
                            ctx
                                |> setInputsAndAddRedeemers [(fundHolding_UTxO_With_Deposits_MockData tp, FundHoldingT.mkCollect_Managers_CommissionRedeemer (tpCollectCommissionsDate tp) withdraw_Commissions_MockData')]
                                |> setOutputs [fundHolding_UTxO_With_Collected_Managers tp withdraw_Commissions_MockData']
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isCommissionsAvailable"]
                , Tasty.testCase "Trying to collect commissions without adding admin signatory must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setSignatories []
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isSignedByAny admins nor isAdminTokenPresent"]
                , Tasty.testCase "Collecting commissions outside valid range must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setValidyRange (Ledger.interval 0 300_000)
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isDateInRange"]
                ]

fundHolding_Validator_Redeemer_Collect_Delegators_Commission_Tests :: TestParams -> Tasty.TestTree
fundHolding_Validator_Redeemer_Collect_Delegators_Commission_Tests tp =
    let
        ------------------------
        txName = show FundHolding_Collect_Delegators_Commission_Tx
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_Collect_Delegators_Commission_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
    in
        ------------------------

        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
            let
                ctx = fundHolding_Collect_Delegators_Commission_TxContext tp
            in
                [ Tasty.testCase "Collecting commissions correctly must succeed" $ do
                    let
                        ctx' = ctx
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                , Tasty.testCase "Collecting commissions without updating remaining commissions must fail" $ do
                    let
                        modifiedDatumType =
                            (fundHolding_DatumType_With_Deposits_MockData tp)
                                { FundHoldingT.hdSubtotal_FT_Commissions =
                                    FundHoldingT.hdSubtotal_FT_Commissions (fundHolding_DatumType_With_Deposits_MockData tp)
                                }
                        modifiedUTxO =
                            (fundHolding_UTxO_With_Collected_Delegators tp withdraw_Commissions_MockData)
                                { LedgerApiV2.txOutDatum =
                                    LedgerApiV2.OutputDatum $ FundHoldingT.mkDatum modifiedDatumType
                                }
                        ctx' =
                            ctx
                                |> setOutputs [modifiedUTxO]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isCorrect_Output_FundHolding_Datum_With_Collect_Delegators_Commission"]
                , Tasty.testCase "Collecting commissions without updating FundHolding value must fail" $ do
                    let
                        modifiedUTxO =
                            (fundHolding_UTxO_With_Collected_Delegators tp withdraw_Commissions_MockData)
                                { LedgerApiV2.txOutValue =
                                    LedgerApiV2.txOutValue (fundHolding_UTxO_With_Deposits_MockData tp)
                                }
                        ctx' =
                            ctx
                                |> setOutputs [modifiedUTxO]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isCorrect_Output_FundHolding_Value_Without_FT_for_Commissions"]
                , Tasty.testCase "Trying to collect more commissions than the available must fail" $ do
                    let
                        available = FundHelpers.getCommissionsAvailable (tpDeadline tp) (fundHolding_DatumType_With_Deposits_MockData tp) (tpShare_InBPx1e2_Delegators tp) (FundHoldingT.hdSubtotal_FT_Commissions_Collected_Delegators (fundHolding_DatumType_With_Deposits_MockData tp)) (tpCollectCommissionsDate tp)
                        withdraw_Commissions_MockData' = available + 1
                        ctx' =
                            ctx
                                |> setInputsAndAddRedeemers [(fundHolding_UTxO_With_Deposits_MockData tp, FundHoldingT.mkCollect_Delegators_CommissionRedeemer (tpCollectCommissionsDate tp) withdraw_Commissions_MockData')]
                                |> setOutputs [fundHolding_UTxO_With_Collected_Delegators tp withdraw_Commissions_MockData']
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isCommissionsAvailable"]
                , Tasty.testCase "Trying to collect commissions without adding admin signatory must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setSignatories []
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isSignedByAny delegatorsAdmins"]
                , Tasty.testCase "Collecting commissions outside valid range must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setValidyRange (Ledger.interval 0 300_000)
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isDateInRange"]
                ]

--------------------------------------------------------------------------------

fundHolding_Validator_Redeemer_ReIndexing_Tests :: TestParams -> Tasty.TestTree
fundHolding_Validator_Redeemer_ReIndexing_Tests tp =
    let
        ------------------------
        txName = show Fund_ReIndexing_Tx
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_ReIndexing_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
    in
        ------------------------

        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
            let
                ctx = fundHolding_ReIndexing_TxContext tp
            in
                [ Tasty.testCase "Re-index correctly must succeed" $ do
                    let
                        ctx' = ctx
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                , Tasty.testCase "Not including protocol input ref must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setInputsRef []
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["Expected exactly one Fund input ref"]
                , Tasty.testCase "Not including some FundHolding as ref must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setInputsRef [fund_UTxO_MockData tp]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["expected all but one FundHolding as input ref"]
                , Tasty.testCase "Not including invest unit input must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setInputsAndAddRedeemers
                                    [ (fundHolding_UTxO_With_Deposits_MockData tp, FundHoldingT.mkReIndexingRedeemer investUnit_AfterReIdx investUnit_Initial)
                                    ]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["Expected exactly one InvestUnit input"]
                , Tasty.testCase "Not having a FundHolding output must fail" $ do
                    let
                        -- NOTE: pongo a la fuerza cualquier output, para que no falle por no tener outputs
                        ctx' =
                            ctx
                                |> setOutputs [protocol_UTxO_MockData tp]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["Expected FundHolding at output index 0"]
                , Tasty.testCase "Updating FundHolding must fail" $ do
                    let
                        modifiedDatumType =
                            (fundHolding_DatumType_With_Deposits_MockData tp)
                                { FundHoldingT.hdSubtotal_FT_Commissions =
                                    FundHoldingT.hdSubtotal_FT_Commissions (fundHolding_DatumType_With_Deposits_MockData tp) + 1
                                }
                        modifiedUTxO =
                            (fundHolding_UTxO_After_Reidx_MockData tp investUnit_Initial investUnit_AfterReIdx)
                                { LedgerApiV2.txOutDatum =
                                    LedgerApiV2.OutputDatum $ FundHoldingT.mkDatum modifiedDatumType
                                }
                        ctx' =
                            ctx
                                |> setOutputs [modifiedUTxO]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isCorrect_Output_FundHolding_Datum_NotChanged"]
                , Tasty.testCase "Not updating the FundHolding value must fail" $ do
                    let
                        modifiedUTxO =
                            (fundHolding_UTxO_After_Reidx_MockData tp investUnit_Initial investUnit_AfterReIdx)
                                { LedgerApiV2.txOutValue = LedgerApiV2.txOutValue (fundHolding_UTxO_With_Deposits_MockData tp)
                                }
                        ctx' =
                            ctx
                                |> setOutputs [modifiedUTxO]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isCorrect_Output_FundHolding_Value_WithTokensExchanged"]
                , Tasty.testCase "Having an incorrect redeemer for the Invest Unit validator must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setInputsAndAddRedeemers
                                    [ (fundHolding_UTxO_With_Deposits_MockData tp, FundHoldingT.mkReIndexingRedeemer investUnit_AfterReIdx investUnit_Initial)
                                    , (investUnit_UTxO_MockData tp, InvestUnitT.mkReIndexingRedeemer (T.InvestUnit [("cc", "name", 0)]) (T.InvestUnit [("cc", "name", 0)]) (oracleReIdxData tp) (oracleReIdxSignature tp))
                                    ]
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` ["not isCorrect_Redeemer_InvestUnit"]
                ]

--------------------------------------------------------------------------------

fundHolding_Validator_Redeemer_BalanceAssets_Tests :: TestParams -> Tasty.TestTree
fundHolding_Validator_Redeemer_BalanceAssets_Tests tp =
    let
        ------------------------
        txName = show FundHolding_BalanceAssets_Tx
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_BalanceAssets_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
        commissionsFT = [0, 0]
    in
        ------------------------
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
            let
                ctx = fundHolding_BalanceAssets_TxContext tp [deposit_MockData, deposit_MockData] [deposit_MockData, deposit_MockData] commissionsFT
            in
                [ Tasty.testCase "Balancing assets correctly must succeed" $ do
                    let
                        ctx' = ctx
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                , Tasty.testCase "Incorrect redeemer with more items in commissions list must fail" $ do
                    let
                        ctx' = fundHolding_BalanceAssets_TxContext tp [deposit_MockData, deposit_MockData] [deposit_MockData, deposit_MockData] [0, 0, 0]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not len alterCommissionsFT == cantOutputs"]
                , Tasty.testCase "Incorrect redeemer with less items in commissions list invalid must fail" $ do
                    let
                        ctx' = fundHolding_BalanceAssets_TxContext tp [deposit_MockData, deposit_MockData] [deposit_MockData, deposit_MockData] [0]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not len alterCommissionsFT == cantOutputs"]
                , Tasty.testCase "Incorrect redeemer with sum total of commision not zero (plus zero) must fail" $ do
                    let
                        ctx' = fundHolding_BalanceAssets_TxContext tp [deposit_MockData, deposit_MockData] [deposit_MockData, deposit_MockData] [1, 0]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isCorrect_Outputs_Commissions_SameTotal"]
                , Tasty.testCase "Incorrect redeemer with sum total of commision not zero (less zero) must fail" $ do
                    let
                        ctx' = fundHolding_BalanceAssets_TxContext tp [deposit_MockData, deposit_MockData] [deposit_MockData, deposit_MockData] [-1, 0]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isCorrect_Outputs_Commissions_SameTotal"]
                ]

--------------------------------------------------------------------------------

fundHolding_Validator_Redeemer_Delete_Tests :: TestParams -> Tasty.TestTree
fundHolding_Validator_Redeemer_Delete_Tests tp =
    let
        ------------------------
        txName = show FundHolding_Delete_Tx
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_Delete_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
    in
        ------------------------

        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
            let
                ctx = fundHolding_Delete_TxContext tp
            in
                [ Tasty.testCase "Delete FundHolding correctly must succeed" $ do
                    let
                        ctx' = ctx
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                , Tasty.testCase "Not empty FundHolding value must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setInputsAndAddRedeemers
                                    [ (fund_UTxO_With_Added_FundHolding_MockData tp, FundT.mkFundHoldingDeleteRedeemer)
                                    , (fundHolding_UTxO_With_Deposits_MockData tp, FundHoldingT.mkDeleteRedeemer)
                                    ]
                    results <- testContextWrapper tp ctx'
                    (Just (RedeemerLogPolicy (Just FundHolding_BurnID_TestRedeemer)), results)
                        `assertResultsContainAnyOf` ["not isZeroAssets"]
                , Tasty.testCase "Not including fund admin sign must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setSignatories []
                    results <- testContextWrapper tp ctx'
                    (Just (RedeemerLogValidator (Just Fund_FundHoldingDelete_TestRedeemer)), results)
                        `assertResultsContainAnyOf` ["not isSignedByAny admins nor isAdminTokenPresent"]
                , Tasty.testCase "Not including Fund UTXO as input must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setInputsAndAddRedeemers [(fundHolding_UTxO_With_NoDeposits_MockData tp, FundHoldingT.mkDeleteRedeemer)]
                    results <- testContextWrapper tp ctx'
                    (Just (RedeemerLogPolicy (Just FundHolding_BurnID_TestRedeemer)), results)
                        `assertResultsContainAnyOf` ["Expected exactly one Fund input"]
                , Tasty.testCase "Not including Fund UTXO as output must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setOutputs [protocol_UTxO_MockData tp]
                    -- NOTE: necesito agregar alguna output para que no falle por no tener outputs
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["Expected Fund at output index 0"]
                , Tasty.testCase "Not burning FundHolding ID must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setMintAndAddRedeemers mempty
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isBurningFundHoldingID"]
                , Tasty.testCase "Too big range must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setValidyRange (createInValidRange (tpTransactionDate tp))
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isValidRange"]
                ]

--------------------------------------------------------------------------------
