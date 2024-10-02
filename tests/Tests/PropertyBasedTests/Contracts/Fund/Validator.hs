--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3
{- |
Module      : Tests.PropertyBasedTests.Fund.Validator
Description : Validation logic and property-based tests related to the Fund
              validator.

This module defines the validation logic for the Fund's contract.

It includes multiple property-based test cases to ensure the integrity and
correctness of the validator script.
-}
module Contracts.Fund.Validator where

--------------------------------------------------------------------------------

-- Non-IOG imports

import qualified Control.Monad.Reader                      as MReader
import           Prelude
import qualified Test.QuickCheck                           as QC
import qualified Test.Tasty                                as Tasty
import qualified Test.Tasty.QuickCheck                     as TastyQC

-- IOG imports
import qualified Ledger.Ada                                as LedgerAda
import qualified Plutus.V2.Ledger.Api                      as LedgerApiV2
import           PlutusTx.Prelude                          (sort)

-- Project imports
import qualified Generic.OffChainHelpers                   as OffChainHelpers
import qualified Generic.Types                             as T
import qualified Protocol.Constants                        as T
import qualified Protocol.Fund.Holding.Types               as FundHoldingT
import qualified Protocol.Fund.Types                       as FundT
import           TestUtils.Contracts.InitialData
import           TestUtils.Contracts.TxContext.Fund
import           TestUtils.HelpersMAYZ
import           TestUtils.QuickCheckGen.QuickCheckGen
import           TestUtils.QuickCheckGen.QuickCheckGenMAYZ
import           TestUtils.TestContext.Asserts
import           TestUtils.TestContext.Helpers
import           TestUtils.Types
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------3

fund_Validator_Tests :: AppM Tasty.TestTree
fund_Validator_Tests = do
    tp <- MReader.asks appTestParams
    return $
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
                        TastyQC.testProperty
                        "Updating admins with arbitrary list should succeed"
                        (prop_updateDatum_updateAdminsOK tp selectedRedeemer ctx)
                    , TastyQC.testProperty
                        "Modifying Fund UTxO value must fail"
                        (prop_updateDatum_changeValueFail tp selectedRedeemer ctx)
                    , TastyQC.testProperty
                        "Modifying datum fields other than admins must fail"
                        (prop_updateDatum_changeDatumFail tp selectedRedeemer ctx)
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
                    --TODO:
                    ctx = fund_UpdateMinADA_TxContext tp toAlter_minAda
                in
                    []

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
                        TastyQC.testProperty
                            "Adding a FundHolding with random creator to random input datum should succeed"
                            (prop_addHolding_addHoldingOK tp selectedRedeemer ctx)
                        , TastyQC.testProperty
                            "Modifying Fund UTxO value must fail"
                            (prop_addHolding_changeValueFail tp selectedRedeemer ctx)
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
                        TastyQC.testProperty
                            "Deleting a FundHolding with random creator from random input datum should succeed"
                            (prop_deleteHolding_deleteHoldingOK tp selectedRedeemer ctx)
                        , TastyQC.testProperty
                            "Modifying Fund UTxO value must fail"
                            (prop_deleteHolding_changeValueFail tp selectedRedeemer ctx)
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
                    []

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
                    --TODO
                    ]

--------------------------------------------------------------------------------

prop_updateDatum_updateAdminsOK :: TestParams -> RedeemerLog -> LedgerApiV2.ScriptContext -> QC.Property
prop_updateDatum_updateAdminsOK tp selectedRedeemer ctx =
    QC.forAll randomAdmins $ \admins -> do
        let
            outputDatumType = (fund_DatumType_MockData tp) {FundT.fdAdmins = sort admins}
            outputFundUTxO =
                (fund_UTxO_MockData tp)
                    { LedgerApiV2.txOutDatum =
                        LedgerApiV2.OutputDatum $ FundT.mkDatum outputDatumType
                    }
            ctx' = ctx
                    |> setOutputs [outputFundUTxO]
        results <- testContextWrapper tp ctx'
        (Just selectedRedeemer, results)
            `assertResultsContainAnyOf` []
    where
        randomAdmins = QC.arbitrary :: QC.Gen [T.WalletPaymentPKH]

prop_updateDatum_changeValueFail :: TestParams -> RedeemerLog -> LedgerApiV2.ScriptContext -> QC.Property
prop_updateDatum_changeValueFail tp selectedRedeemer ctx =
    QC.forAll randomAdminsAndValue $ \(admins, value) -> do
        let
            outputDatumType = (fund_DatumType_MockData tp) {FundT.fdAdmins = sort admins}
            outputFundUTxO =
                (fund_UTxO_MockData tp)
                    { LedgerApiV2.txOutDatum =
                        LedgerApiV2.OutputDatum $ FundT.mkDatum outputDatumType
                    , LedgerApiV2.txOutValue =
                        LedgerApiV2.txOutValue (fund_UTxO_MockData tp) <> value
                    }
            ctx' = ctx
                    |> setOutputs [outputFundUTxO]
        results <- testContextWrapper tp ctx'
        assertResultsContainAnyOfIfCondition (Just selectedRedeemer, results) ["not isCorrect_Output_Fund_Value_NotChanged"] (LedgerApiV2.txOutValue outputFundUTxO /= LedgerApiV2.txOutValue (fund_UTxO_MockData tp))
    where
        randomAdminsAndValue :: QC.Gen ([LedgerApiV2.PubKeyHash], LedgerApiV2.Value)
        randomAdminsAndValue = do
            randomAdmins <- QC.arbitrary :: QC.Gen [LedgerApiV2.PubKeyHash]
            randomValue <- varyValue $ LedgerApiV2.txOutValue (fund_UTxO_MockData tp)
            return (randomAdmins, randomValue)

prop_updateDatum_changeDatumFail :: TestParams -> RedeemerLog -> LedgerApiV2.ScriptContext -> QC.Property
prop_updateDatum_changeDatumFail tp selectedRedeemer ctx =
    QC.forAll (genValidFundDatumType tp) $ \datumType -> do
        let
            outputFundUTxO =
                (fund_UTxO_MockData tp)
                    { LedgerApiV2.txOutDatum =
                        LedgerApiV2.OutputDatum $ FundT.mkDatum datumType
                    }
            controlDatumType = (fund_DatumType_MockData tp) {FundT.fdAdmins = FundT.fdAdmins datumType}
            ctx' = ctx
                    |> setOutputs [outputFundUTxO]
        results <- testContextWrapper tp ctx'
        assertResultsContainAnyOfIfCondition (Just selectedRedeemer, results) ["error1"]  (controlDatumType /= datumType)

prop_addHolding_addHoldingOK :: TestParams -> RedeemerLog -> LedgerApiV2.ScriptContext -> QC.Property
prop_addHolding_addHoldingOK tp selectedRedeemer ctx =
    -- DebugTrace.trace ("Testing value: " ++ show inputDatumType) $
    QC.forAll (genValidFundDatumType tp) $ \datumType -> do
        let
            inputFundUTxO =
                LedgerApiV2.TxOut
                    (OffChainHelpers.addressValidator $ FundT.fdFundValidator_Hash (fund_DatumType_MockData tp))
                    ( LedgerAda.lovelaceValueOf minAdaFundDatum
                        <> LedgerApiV2.singleton (FundT.fdFundPolicy_CS datumType) T.fundID_TN 1
                        <> LedgerApiV2.singleton (tpTokenMAYZ_CS tp) (tpTokenMAYZ_TN tp) 7
                    )
                    (LedgerApiV2.OutputDatum $ FundT.mkDatum datumType)
                    Nothing

            outputDatumType =
                datumType
                    { FundT.fdHoldingsCount =
                        FundT.fdHoldingsCount datumType + 1
                    , FundT.fdHoldingsIndex =
                        FundT.fdHoldingsIndex datumType + 1
                    }

            outputFundUTxO =
                inputFundUTxO
                    { LedgerApiV2.txOutDatum =
                        LedgerApiV2.OutputDatum $ FundT.mkDatum outputDatumType
                    }

            ctx' = ctx
                    |> setInputsAndAddRedeemers [(inputFundUTxO, FundT.mkFundHoldingAddRedeemer)]
                    |> setOutputs [outputFundUTxO, fundHolding_UTxO_With_NoDeposits_MockData tp]
                    |> setSignatories [head $ FundT.fdAdmins datumType]
                    |> setMintAndAddRedeemers
                        [( LedgerApiV2.singleton
                            (FundT.fdFundHoldingPolicyID_CS datumType)
                            (mkFundHoldingID_TN 0)
                            1
                        , FundHoldingT.mkMintIDRedeemer)]
        results <- testContextWrapper tp ctx'
        (Just selectedRedeemer, results)
            `assertResultsContainAnyOf` []

prop_addHolding_changeValueFail :: TestParams -> RedeemerLog -> LedgerApiV2.ScriptContext -> QC.Property
prop_addHolding_changeValueFail tp selectedRedeemer ctx =
    QC.forAll (genValidFundDatumType tp) $ \datumType ->
        QC.forAll randomValue $ \value -> do
            let
                inputFundUTxO =
                    LedgerApiV2.TxOut
                        (OffChainHelpers.addressValidator $ FundT.fdFundValidator_Hash (fund_DatumType_MockData tp))
                        ( LedgerAda.lovelaceValueOf minAdaFundDatum
                            <> LedgerApiV2.singleton (FundT.fdFundPolicy_CS datumType) T.fundID_TN 1
                            <> LedgerApiV2.singleton (tpTokenMAYZ_CS tp) (tpTokenMAYZ_TN tp) 7
                        )
                        (LedgerApiV2.OutputDatum $ FundT.mkDatum datumType)
                        Nothing

                outputDatumType =
                    datumType
                        { FundT.fdHoldingsCount =
                            FundT.fdHoldingsCount datumType + 1
                        , FundT.fdHoldingsIndex =
                            FundT.fdHoldingsIndex datumType + 1
                        }

                outputFundUTxO =
                    inputFundUTxO
                        { LedgerApiV2.txOutDatum =
                            LedgerApiV2.OutputDatum $ FundT.mkDatum outputDatumType
                        , LedgerApiV2.txOutValue =
                            LedgerApiV2.txOutValue inputFundUTxO <> value
                        }

                ctx' = ctx
                        |> setInputsAndAddRedeemers [(inputFundUTxO, FundT.mkFundHoldingAddRedeemer)]
                        |> setOutputs [outputFundUTxO]
                        |> setSignatories [head $ FundT.fdAdmins datumType]
                        |> setMintAndAddRedeemers
                            [( LedgerApiV2.singleton
                                (FundT.fdFundHoldingPolicyID_CS datumType)
                                (mkFundHoldingID_TN 0)
                                1
                            , FundHoldingT.mkMintIDRedeemer)]
            results <- testContextWrapper tp ctx'
            assertResultsContainAnyOfIfCondition (Just selectedRedeemer, results) ["not isCorrect_Output_Fund_Value_NotChanged"]  (LedgerApiV2.txOutValue outputFundUTxO /= LedgerApiV2.txOutValue inputFundUTxO)
    where
        randomValue = varyValue $ LedgerApiV2.txOutValue (fund_UTxO_MockData tp)

prop_deleteHolding_deleteHoldingOK :: TestParams -> RedeemerLog -> LedgerApiV2.ScriptContext -> QC.Property
prop_deleteHolding_deleteHoldingOK tp selectedRedeemer ctx =
    QC.forAll (genValidFundDatumType tp) $ \datumType -> do
        let
            inputFundUTxO =
                LedgerApiV2.TxOut
                    (OffChainHelpers.addressValidator $ FundT.fdFundValidator_Hash (fund_DatumType_MockData tp))
                    ( LedgerAda.lovelaceValueOf minAdaFundDatum
                        <> LedgerApiV2.singleton (FundT.fdFundPolicy_CS datumType) T.fundID_TN 1
                        <> LedgerApiV2.singleton (tpTokenMAYZ_CS tp) (tpTokenMAYZ_TN tp) 7
                    )
                    (LedgerApiV2.OutputDatum $ FundT.mkDatum datumType)
                    Nothing

            outputDatumType =
                datumType
                    { FundT.fdHoldingsCount =
                        FundT.fdHoldingsCount datumType - 1
                    }

            outputFundUTxO =
                inputFundUTxO
                    { LedgerApiV2.txOutDatum =
                        LedgerApiV2.OutputDatum $ FundT.mkDatum outputDatumType
                    }

            ctx'= ctx
                    |> setInputsAndAddRedeemers [(inputFundUTxO, FundT.mkFundHoldingDeleteRedeemer), (fundHolding_UTxO_With_NoDeposits_MockData tp, FundHoldingT.mkDeleteRedeemer)]
                    |> setOutputs [outputFundUTxO]
                    |> setSignatories [head $ FundT.fdAdmins datumType]
                    |> setMintAndAddRedeemers
                       [ ( LedgerApiV2.singleton
                            (FundT.fdFundHoldingPolicyID_CS datumType)
                            (mkFundHoldingID_TN 0)
                            (-1)
                        , FundHoldingT.mkBurnIDRedeemer)]
        results <- testContextWrapper tp ctx'
        (Just selectedRedeemer, results)
            `assertResultsContainAnyOf` []

prop_deleteHolding_changeValueFail :: TestParams -> RedeemerLog -> LedgerApiV2.ScriptContext -> QC.Property
prop_deleteHolding_changeValueFail tp selectedRedeemer ctx =
    QC.forAll (genValidFundDatumType tp) $ \datumType ->
        QC.forAll randomValue $ \value -> do
            let
                inputFundUTxO =
                    LedgerApiV2.TxOut
                        (OffChainHelpers.addressValidator $ FundT.fdFundValidator_Hash (fund_DatumType_MockData tp))
                        ( LedgerAda.lovelaceValueOf minAdaFundDatum
                            <> LedgerApiV2.singleton (FundT.fdFundPolicy_CS datumType) T.fundID_TN 1
                            <> LedgerApiV2.singleton (tpTokenMAYZ_CS tp) (tpTokenMAYZ_TN tp) 7
                        )
                        (LedgerApiV2.OutputDatum $ FundT.mkDatum datumType)
                        Nothing

                outputDatumType =
                    datumType
                        { FundT.fdHoldingsCount =
                            FundT.fdHoldingsCount datumType - 1
                        }

                outputFundUTxO =
                    inputFundUTxO
                        { LedgerApiV2.txOutDatum =
                            LedgerApiV2.OutputDatum $ FundT.mkDatum outputDatumType
                        , LedgerApiV2.txOutValue =
                            LedgerApiV2.txOutValue inputFundUTxO <> value
                        }

                ctx' = ctx
                        |> setInputsAndAddRedeemers [(inputFundUTxO, FundT.mkFundHoldingDeleteRedeemer)]
                        |> setOutputs [outputFundUTxO]
                        |> setSignatories [head $ FundT.fdAdmins datumType]
                        |> setMintAndAddRedeemers
                            [ ( LedgerApiV2.singleton
                            (FundT.fdFundHoldingPolicyID_CS datumType)
                            (mkFundHoldingID_TN 0)
                            (-1)
                            , FundHoldingT.mkBurnIDRedeemer)]
            results <- testContextWrapper tp ctx'
            assertResultsContainAnyOfIfCondition (Just selectedRedeemer, results) ["not isCorrect_Output_Fund_Value_NotChanged"] (LedgerApiV2.txOutValue outputFundUTxO /= LedgerApiV2.txOutValue inputFundUTxO)
    where
        randomValue = varyValue $ LedgerApiV2.txOutValue (fund_UTxO_MockData tp)
