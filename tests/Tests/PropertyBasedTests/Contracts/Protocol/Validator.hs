--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4

{- |
Module      : Tests.PropertyBasedTests.Protocol.Validator
Description : Validation logic and property-based tests related to the Protocol
              validator.

This module defines the validation logic for the Protocol's contract.

It includes multiple property-based test cases to ensure the integrity and
correctness of the validator script.
-}
module Contracts.Protocol.Validator where
--------------------------------------------------------------------------------
-- Non-IOG imports
import qualified Control.Monad.Reader                      as MReader
import qualified Data.Text
import           Prelude
import qualified Test.QuickCheck                           as QC
import qualified Test.Tasty                                as Tasty
import qualified Test.Tasty.QuickCheck                     as TastyQC

-- IOG imports
import qualified Ledger.Ada                                as LedgerAda
import qualified Ledger.Address                            as LedgerAddress
import qualified Ledger.Value                              as LedgerValue
import qualified Plutus.V2.Ledger.Api                      as LedgerApiV2
import qualified Test.Tasty.HUnit                          as Tasty

-- Project imports
import qualified Protocol.Constants                        as T
import qualified Protocol.Protocol.Types                   as ProtocolT
import           TestUtils.HelpersMAYZ
import           TestUtils.QuickCheckGen.QuickCheckGen
import           TestUtils.TestContext.Asserts
import           TestUtils.TestContext.Helpers
import           TestUtils.Types

import           TestUtils.Contracts.InitialData
import           TestUtils.Contracts.TxContext.Protocol
import           TestUtils.QuickCheckGen.QuickCheckGenMAYZ
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------

_CHANGE_VALUE :: Bool
_CHANGE_VALUE = True
_NO_CHANGE_VALUE :: Bool
_NO_CHANGE_VALUE = False
_VALID_VALUE_MINADA_CHANGE :: Bool
_VALID_VALUE_MINADA_CHANGE = True
_INVALID_VALUE_MINADA_CHANGE :: Bool
_INVALID_VALUE_MINADA_CHANGE = False
_CHANGE_DATUM :: Bool
_CHANGE_DATUM = True
_NO_CHANGE_DATUM :: Bool
_NO_CHANGE_DATUM = False
_VALID_DATUM_CHANGE :: Bool
_VALID_DATUM_CHANGE = False
_INVALID_DATUM_CHANGE :: Bool
_INVALID_DATUM_CHANGE = True
_VALID_SIGNATURE :: Bool
_VALID_SIGNATURE = True
_INVALID_SIGNATURE :: Bool
_INVALID_SIGNATURE = False
_USE_ADMIN_TOKEN :: Bool
_USE_ADMIN_TOKEN = True
_DONT_USE_ADMIN_TOKEN :: Bool
_DONT_USE_ADMIN_TOKEN = False
_USE_EMERGENCY_TOKEN :: Bool
_USE_EMERGENCY_TOKEN = True
_DONT_USE_EMERGENCY_TOKEN :: Bool
_DONT_USE_EMERGENCY_TOKEN = False
_VALID_TOKEN_INDEX :: Bool
_VALID_TOKEN_INDEX = True
_INVALID_TOKEN_INDEX :: Bool
_INVALID_TOKEN_INDEX = False

--------------------------------------------------------------------------------

protocol_Validator_Tests :: AppM Tasty.TestTree
protocol_Validator_Tests = do
    tp <- MReader.asks appTestParams
    return $
        Tasty.testGroup
            "Protocol Validator Tests"
            [ protocol_Validator_Redeemer_DatumUpdate_Tests tp
            , protocol_Validator_Redeemer_UpdateMinADA_Tests tp
            , protocol_Validator_Redeemer_Emergency_Tests tp
            ]

--------------------------------------------------------------------------------

protocol_Validator_Redeemer_DatumUpdate_Tests :: TestParams -> Tasty.TestTree
protocol_Validator_Redeemer_DatumUpdate_Tests tp =
    let
        ------------------------
        txName = show Protocol_DatumUpdate_Tx
        selectedRedeemer = RedeemerLogValidator (Just Protocol_DatumUpdate_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = protocol_DatumUpdate_TxContext tp [] "aaff"
                in
                    [
                        TastyQC.testProperty "Changing the Protocol UTxO value must fail" $ do createTestCase tp selectedRedeemer ctx ProtocolT.mkDatumUpdateRedeemer _CHANGE_VALUE _INVALID_VALUE_MINADA_CHANGE _NO_CHANGE_DATUM _VALID_DATUM_CHANGE _VALID_SIGNATURE _DONT_USE_ADMIN_TOKEN _DONT_USE_EMERGENCY_TOKEN _VALID_TOKEN_INDEX "not isCorrect_Output_Protocol_Value_NotChanged"
                        , TastyQC.testProperty "Changing the Protocol Datum with wrong signature must fail" $ do createTestCase tp selectedRedeemer ctx ProtocolT.mkDatumUpdateRedeemer _NO_CHANGE_VALUE _INVALID_VALUE_MINADA_CHANGE _CHANGE_DATUM _VALID_DATUM_CHANGE _INVALID_SIGNATURE _DONT_USE_ADMIN_TOKEN _DONT_USE_EMERGENCY_TOKEN _VALID_TOKEN_INDEX "not isSignedByAny admins nor isAdminTokenPresent"
                        , TastyQC.testProperty "Changing the Protocol Datum with correct signature must succeed" $ do createTestCase tp selectedRedeemer ctx ProtocolT.mkDatumUpdateRedeemer _NO_CHANGE_VALUE _INVALID_VALUE_MINADA_CHANGE _CHANGE_DATUM _VALID_DATUM_CHANGE _VALID_SIGNATURE _DONT_USE_ADMIN_TOKEN _DONT_USE_EMERGENCY_TOKEN _VALID_TOKEN_INDEX ""
                        , TastyQC.testProperty "Changing the Protocol Datum with correct signature but invalid data must fail" $ do createTestCase tp selectedRedeemer ctx ProtocolT.mkDatumUpdateRedeemer _NO_CHANGE_VALUE _INVALID_VALUE_MINADA_CHANGE _CHANGE_DATUM _INVALID_DATUM_CHANGE _VALID_SIGNATURE _DONT_USE_ADMIN_TOKEN _DONT_USE_EMERGENCY_TOKEN _VALID_TOKEN_INDEX "not isCorrect_Output_Protocol_Datum_Updated"
                        , TastyQC.testProperty "Changing the Protocol Datum with no signature but With Admin Token must succeed" $ do createTestCase tp selectedRedeemer ctx ProtocolT.mkDatumUpdateRedeemer _NO_CHANGE_VALUE _INVALID_VALUE_MINADA_CHANGE _CHANGE_DATUM _VALID_DATUM_CHANGE _INVALID_SIGNATURE _USE_ADMIN_TOKEN _DONT_USE_EMERGENCY_TOKEN _VALID_TOKEN_INDEX ""
                        , TastyQC.testProperty "Changing the Protocol Datum with no signature but With Admin Token in incorrect output index must fail" $ do createTestCase tp selectedRedeemer ctx ProtocolT.mkDatumUpdateRedeemer _NO_CHANGE_VALUE _INVALID_VALUE_MINADA_CHANGE _CHANGE_DATUM _VALID_DATUM_CHANGE _INVALID_SIGNATURE _USE_ADMIN_TOKEN _DONT_USE_EMERGENCY_TOKEN _INVALID_TOKEN_INDEX "not isSignedByAny admins nor isAdminTokenPresent"
                        , TastyQC.testProperty "Changing the Protocol Datum with no signature no Admin Token but With Emergency Token must fail" $ do createTestCase tp selectedRedeemer ctx ProtocolT.mkDatumUpdateRedeemer _NO_CHANGE_VALUE _INVALID_VALUE_MINADA_CHANGE _CHANGE_DATUM _VALID_DATUM_CHANGE _INVALID_SIGNATURE _DONT_USE_ADMIN_TOKEN _USE_EMERGENCY_TOKEN _VALID_TOKEN_INDEX "not isSignedByAny admins nor isAdminTokenPresent"
                    ]

protocol_Validator_Redeemer_UpdateMinADA_Tests :: TestParams -> Tasty.TestTree
protocol_Validator_Redeemer_UpdateMinADA_Tests tp =
    let
        ------------------------
        txName = show Protocol_UpdateMinADA_Tx
        selectedRedeemer = RedeemerLogValidator (Just Protocol_UpdateMinADA_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = protocol_UpdateMinADA_TxContext tp toAlter_minAda
                in
                   [
                        TastyQC.testProperty "Changing the Protocol UTxO value min ADA with wrong signature must fail" $ do createTestCase tp selectedRedeemer ctx ProtocolT.mkUpdateMinADARedeemer _CHANGE_VALUE _VALID_VALUE_MINADA_CHANGE _NO_CHANGE_DATUM _VALID_DATUM_CHANGE _INVALID_SIGNATURE _DONT_USE_ADMIN_TOKEN _DONT_USE_EMERGENCY_TOKEN _VALID_TOKEN_INDEX "not isSignedByAny admins nor isAdminTokenPresent"
                        , TastyQC.testProperty "Changing the Protocol UTxO value min ADA with signature must succeed" $ do createTestCase tp selectedRedeemer ctx ProtocolT.mkUpdateMinADARedeemer _CHANGE_VALUE _VALID_VALUE_MINADA_CHANGE _NO_CHANGE_DATUM _VALID_DATUM_CHANGE _VALID_SIGNATURE _DONT_USE_ADMIN_TOKEN _DONT_USE_EMERGENCY_TOKEN _VALID_TOKEN_INDEX ""
                        , TastyQC.testProperty "Changing the Protocol UTxO value min ADA with signature but valid data change must fail" $ do createTestCase tp selectedRedeemer ctx ProtocolT.mkUpdateMinADARedeemer _CHANGE_VALUE _VALID_VALUE_MINADA_CHANGE _CHANGE_DATUM _VALID_DATUM_CHANGE _VALID_SIGNATURE _DONT_USE_ADMIN_TOKEN _DONT_USE_EMERGENCY_TOKEN _VALID_TOKEN_INDEX "not isCorrect_Output_Protocol_Datum_With_MinADAChanged"
                        , TastyQC.testProperty "Changing the Protocol UTxO value min ADA with signature and invalid value must fail" $ do createTestCase tp selectedRedeemer ctx ProtocolT.mkUpdateMinADARedeemer _CHANGE_VALUE _INVALID_VALUE_MINADA_CHANGE _NO_CHANGE_DATUM _VALID_DATUM_CHANGE _VALID_SIGNATURE _DONT_USE_ADMIN_TOKEN _DONT_USE_EMERGENCY_TOKEN _VALID_TOKEN_INDEX "not isCorrect_Output_Protocol_Value_With_MinADAChanged"
                        , TastyQC.testProperty "Changing the Protocol UTxO value min ADA with wrong signature but admin token must succeed" $ do createTestCase tp selectedRedeemer ctx ProtocolT.mkUpdateMinADARedeemer _CHANGE_VALUE _VALID_VALUE_MINADA_CHANGE _NO_CHANGE_DATUM _VALID_DATUM_CHANGE _INVALID_SIGNATURE _USE_ADMIN_TOKEN _DONT_USE_EMERGENCY_TOKEN _VALID_TOKEN_INDEX ""
                        , TastyQC.testProperty "Changing the Protocol UTxO value min ADA with wrong signature but emergency token must fail" $ do createTestCase tp selectedRedeemer ctx ProtocolT.mkUpdateMinADARedeemer _CHANGE_VALUE _VALID_VALUE_MINADA_CHANGE _NO_CHANGE_DATUM _VALID_DATUM_CHANGE _INVALID_SIGNATURE _DONT_USE_ADMIN_TOKEN _USE_EMERGENCY_TOKEN _VALID_TOKEN_INDEX "not isSignedByAny admins nor isAdminTokenPresent"
                    ]

protocol_Validator_Redeemer_Emergency_Tests :: TestParams -> Tasty.TestTree
protocol_Validator_Redeemer_Emergency_Tests tp =
    let
        ------------------------
        txName = show Protocol_Emergency_Tx
        selectedRedeemer = RedeemerLogValidator (Just Protocol_Emergency_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = protocol_UpdateMinADA_TxContext tp toAlter_minAda
                in
                   [
                        TastyQC.testProperty "Changing the Protocol UTxO value With Emergency Token must succeed" $ do createTestCase tp selectedRedeemer ctx ProtocolT.mkEmergencyRedeemer _CHANGE_VALUE _INVALID_VALUE_MINADA_CHANGE _NO_CHANGE_DATUM _VALID_DATUM_CHANGE _INVALID_SIGNATURE _DONT_USE_ADMIN_TOKEN _USE_EMERGENCY_TOKEN _VALID_TOKEN_INDEX ""
                        , TastyQC.testProperty "Changing the Protocol UTxO value With Emergency Token in incorrect output index must fail" $ do createTestCase tp selectedRedeemer ctx ProtocolT.mkEmergencyRedeemer _CHANGE_VALUE _INVALID_VALUE_MINADA_CHANGE _NO_CHANGE_DATUM _VALID_DATUM_CHANGE _INVALID_SIGNATURE _DONT_USE_ADMIN_TOKEN _USE_EMERGENCY_TOKEN _INVALID_TOKEN_INDEX "not isEmergencyAdminTokenPresent"
                        , TastyQC.testProperty "Changing the Protocol Datum with wrong signature must fail" $ do createTestCase tp selectedRedeemer ctx ProtocolT.mkEmergencyRedeemer _NO_CHANGE_VALUE _INVALID_VALUE_MINADA_CHANGE _CHANGE_DATUM _VALID_DATUM_CHANGE _INVALID_SIGNATURE _DONT_USE_ADMIN_TOKEN _DONT_USE_EMERGENCY_TOKEN _VALID_TOKEN_INDEX "not isEmergencyAdminTokenPresent"
                        , TastyQC.testProperty "Changing the Protocol Datum with wrong signature With Emergency Token must succeed" $ do createTestCase tp selectedRedeemer ctx ProtocolT.mkEmergencyRedeemer _NO_CHANGE_VALUE _INVALID_VALUE_MINADA_CHANGE _CHANGE_DATUM _VALID_DATUM_CHANGE _INVALID_SIGNATURE _DONT_USE_ADMIN_TOKEN _USE_EMERGENCY_TOKEN _VALID_TOKEN_INDEX ""
                        , TastyQC.testProperty "Changing the Protocol Datum With Emergency Token but invalid data must succeed" $ do createTestCase tp selectedRedeemer ctx ProtocolT.mkEmergencyRedeemer _NO_CHANGE_VALUE _INVALID_VALUE_MINADA_CHANGE _CHANGE_DATUM _INVALID_DATUM_CHANGE _INVALID_SIGNATURE _DONT_USE_ADMIN_TOKEN _USE_EMERGENCY_TOKEN _VALID_TOKEN_INDEX ""
                    ]

--------------------------------------------------------------------------------
-- Property-Based Tests
--------------------------------------------------------------------------------

createTestCase :: TestParams -> RedeemerLog -> LedgerApiV2.ScriptContext -> LedgerApiV2.Redeemer -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Data.Text.Text -> QC.Property
createTestCase tp selectedRedeemer ctx redeemer swChangeValue swChangeValueValid swChangeDatum swChangeDatumInvalid swValidSignature swAdminToken swEmergencyToken swValidTokenIndexPosition expectedResult =
    QC.forAll (genValidProtocolDatumType tp) $ \validProtocolDatumType ->
        QC.forAll QC.arbitrary $ \randomSingleton -> do
            let -----------------------
                input_Protocol_UTxO = protocol_UTxO_MockData tp
                input_Protocol_Datum = ProtocolT.getProtocol_DatumType_From_UTxO input_Protocol_UTxO
                -----------------------
                currentMinADA = ProtocolT.pdMinADA input_Protocol_Datum
                newMinADA = 10_000_000
                -----------------------
                outputProtocolDatumType' =
                    if swChangeDatum
                        then
                            if swChangeDatumInvalid
                                then validProtocolDatumType {ProtocolT.pdShare_InBPx1e2_Protocol = 1_000_000}
                                else validProtocolDatumType
                        else input_Protocol_Datum
                outputProtocolDatumType =
                    if swChangeValue && swChangeValueValid
                        then outputProtocolDatumType' {ProtocolT.pdMinADA = newMinADA}
                        else outputProtocolDatumType'
                -----------------------
                outputProtocolDatum = ProtocolT.mkDatum outputProtocolDatumType
                -----------------------
                newValue =
                    if swChangeValue
                        then
                            if swChangeValueValid
                                then LedgerApiV2.txOutValue input_Protocol_UTxO <> LedgerAda.lovelaceValueOf (newMinADA - currentMinADA)
                                else LedgerApiV2.txOutValue input_Protocol_UTxO <> getRandomSingleton randomSingleton
                        else LedgerApiV2.txOutValue input_Protocol_UTxO
                -----------------------
                outputProtocolUTxO = input_Protocol_UTxO {LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum outputProtocolDatum, LedgerApiV2.txOutValue = newValue}
                -----------------------
                tokenAdmin_Value = LedgerValue.singleton (tpTokenAdminPolicy_CS tp) T.protocolTokenAdmin_TN 1
                tokenEmergency_Value = LedgerValue.singleton (tpTokenEmergencyAdminPolicy_CS tp) T.protocolTokenEmergencyAdmin_TN 1
                outputToWalletWithAdminTokenUTxO = LedgerApiV2.TxOut {LedgerApiV2.txOutAddress = LedgerAddress.Address {LedgerApiV2.addressCredential = LedgerApiV2.PubKeyCredential $ LedgerApiV2.PubKeyHash "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e", LedgerApiV2.addressStakingCredential = Nothing}, LedgerApiV2.txOutDatum = LedgerApiV2.NoOutputDatum, LedgerApiV2.txOutValue = LedgerAda.lovelaceValueOf minAdaForUTxOWithTokens <> tokenAdmin_Value, LedgerApiV2.txOutReferenceScript = Nothing}
                outputToWalletWithEmergencyTokenUTxO = LedgerApiV2.TxOut {LedgerApiV2.txOutAddress = LedgerAddress.Address {LedgerApiV2.addressCredential = LedgerApiV2.PubKeyCredential $ LedgerApiV2.PubKeyHash "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e", LedgerApiV2.addressStakingCredential = Nothing}, LedgerApiV2.txOutDatum = LedgerApiV2.NoOutputDatum, LedgerApiV2.txOutValue = LedgerAda.lovelaceValueOf minAdaForUTxOWithTokens <> tokenEmergency_Value, LedgerApiV2.txOutReferenceScript = Nothing}
                -----------------------
                tokenOutputs = ([outputToWalletWithAdminTokenUTxO | swAdminToken]) ++ ([outputToWalletWithEmergencyTokenUTxO | swEmergencyToken])
                orderedOutputs = if swValidTokenIndexPosition then tokenOutputs ++ [outputProtocolUTxO] else outputProtocolUTxO : tokenOutputs
                -----------------------
                ctx' = ctx
                        |> setSignatories (if swValidSignature then tpProtocolAdmins tp else [])
                        |> setInputsAndAddRedeemers [(input_Protocol_UTxO, redeemer)]
                        |> setOutputs orderedOutputs
                        |> setValidyRange (createValidRange (tpTransactionDate tp))
                -----------------------
            results <- testContextWrapper tp ctx'
            let actualErrors = findLogsByKey (Just selectedRedeemer) results
                resultCheck = if expectedResult == "" then null actualErrors else expectedResult `elem` actualErrors
            Tasty.assertBool
                ("Expected: " ++ show expectedResult ++ " - Actual: " ++ show actualErrors)
                resultCheck
                --  || DebugTrace.trace
                --     ("Testing currentMinADA: " ++ show currentMinADA)
                --     ( DebugTrace.trace
                --         ("Testing newMinADA: " ++ show newMinADA)
                --         ( DebugTrace.trace
                --             ("Testing newValue: " ++ show newValue)
                --             ( DebugTrace.trace ("Testing results: " ++ show results ++ " - Testing expectedResult: " ++ show expectedResult ++ " - Testing resultCheck: " ++ show resultCheck) False
                --             )
                --         )
                --     ))

--------------------------------------------------------------------------------
