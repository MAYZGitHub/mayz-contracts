
--------------------------------------------------------------------------------2
{- HLINT ignore "Use tuple-section"          -}
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module TestUtils.Automatic.TxGenerators where

--------------------------------------------------------------------------------

-- Non-IOG imports

import qualified Data.Maybe                            as DataMaybe
import qualified GHC.Stack                             as GHC
import           Prelude                               as P hiding ((<>))
import qualified Test.Tasty                            as Tasty
import qualified Test.Tasty.HUnit                      as Tasty

-- IOG imports

import qualified Ledger.Ada                            as LedgerAda
import qualified Ledger.Address                        as LedgerAddress
import qualified Ledger.Value                          as LedgerValue
import qualified Plutus.V2.Ledger.Api                  as LedgerApiV2
import           PlutusTx.Prelude                      ((<>))

-- Project imports

import qualified Generic.Types                         as T
import           TestUtils.Automatic.ContextGenerator
import           TestUtils.Automatic.Helpers
import           TestUtils.Automatic.HelpersMAYZ
import           TestUtils.Automatic.TestCaseGenerator
import           TestUtils.Automatic.TestCases
import           TestUtils.Automatic.Types
import           TestUtils.HelpersMAYZ
import           TestUtils.TestContext.Asserts
import           TestUtils.TestContext.Helpers
import           TestUtils.Types
import           TestUtils.TypesMAYZ

----------------------------------------------------------------------------------------

transaction_Tests_Gen :: GHC.HasCallStack => TestParams -> RedeemerLog -> String -> TxSpecsParametrizable -> [TxParam] -> TxParamGeneratorsList -> RedeemerTestConfigTree -> TestCaseParams -> Tasty.TestTree
transaction_Tests_Gen tp selectedRedeemer txName txSpecs txParams_Default txParamsGenerators_List redeemerTestConfigTree defaultTestCaseParams=
    let
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        defaultTxSpecs = txSpecs txParams_Default
    in
        Tasty.testGroup
                ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests")
                [
                    inputsRef_Tests_Gen tp selectedRedeemer txName defaultTxSpecs redeemerTestConfigTree defaultTestCaseParams,
                    inputs_Tests_Gen tp selectedRedeemer txName defaultTxSpecs redeemerTestConfigTree defaultTestCaseParams,
                    outputs_Tests_Gen tp selectedRedeemer txName defaultTxSpecs redeemerTestConfigTree defaultTestCaseParams,
                    mints_Tests_Gen tp selectedRedeemer txName defaultTxSpecs redeemerTestConfigTree defaultTestCaseParams,
                    validyRange_Tests_Gen tp selectedRedeemer txName defaultTxSpecs redeemerTestConfigTree defaultTestCaseParams,
                    signatures_Tests_Gen tp selectedRedeemer txName defaultTxSpecs redeemerTestConfigTree defaultTestCaseParams,
                    extras_Tests_Gen tp selectedRedeemer txName defaultTxSpecs redeemerTestConfigTree defaultTestCaseParams,
                    extras_Property_Tests_Gen tp selectedRedeemer txName txSpecs defaultTxSpecs txParamsGenerators_List redeemerTestConfigTree defaultTestCaseParams
                ]

----------------------------------------------------------------------------------------

inputsRef_Tests_Gen :: TestParams -> RedeemerLog -> String -> TxSpecs -> RedeemerTestConfigTree -> TestCaseParams -> Tasty.TestTree
inputsRef_Tests_Gen tp selectedRedeemer txName txSpecs redeemerTestConfigTree defaultTestCaseParams =
    let
        tests =
            [inputRef_Tests_Gen tp selectedRedeemer txName txSpecs redeemerTestConfigTree defaultTestCaseParams (P.fromIntegral index) | index <- [0.. P.length (tsInputsRef txSpecs) -1]]
    in
        Tasty.testGroup
            "Inputs Ref Tests"
            tests

---------------------------

inputRef_Tests_Gen :: TestParams
                   -> RedeemerLog
                   -> String
                   -> TxSpecs
                   -> RedeemerTestConfigTree
                   -> TestCaseParams
                   -> Integer
                   -> Tasty.TestTree
inputRef_Tests_Gen tp selectedRedeemer txName txSpecs redeemerTestConfigTree defaultTestCaseParams indexEntity =
    let
        ----------------------
        inputsRefTestConfigTree = tctInputsRef redeemerTestConfigTree
        entityTestConfigTree = inputsRefTestConfigTree !! fromIntegral indexEntity
        ----------------------
        entity = fst $ tsInputsRef txSpecs !! fromIntegral indexEntity
        otherTokensConfig = testInvalidValueOtherTokens $ getEntityTestConfig tp entity
        ----------------------
        entityName = pretty entity
        ----------------------
        inputRefTest :: String
             -> InputRefOptions
             -> String
             -> [Tasty.TestTree]
        inputRefTest desc inputRefOption configPath =
            case generateTestCase
                tp
                selectedRedeemer
                (P.const txSpecs)
                []
                defaultTestCaseParams
                entityTestConfigTree
                (desc ++ " for " ++ entityName ++ " input ref")
                (setTcInputRefAtIndex indexEntity inputRefOption)
                configPath
                (txName ++ ", " ++ getRedeemerScriptNameFromLog selectedRedeemer ++ ", " ++ getRedeemerNameFromLog selectedRedeemer ++ ", InputsRef, " ++ entityName ++ ".")
                of
                    Nothing -> []
                    Just x  -> [x]
        ----------------------
        -- Generate tests for invalid none and more
        basicTests =
            [
                inputRefTest "Having valid" InputRefValid "InputRefValid",
                inputRefTest "None valid" (InputRefInvalid TxOutInvalidNone) "InputRefInvalid.TxOutInvalidNone",
                inputRefTest "Having more than one" (InputRefInvalid TxOutInvalidMore) "InputRefInvalid.TxOutInvalidMore"
            ]

        -- Value tests
        valueTests =
            [ inputRefTest "Having invalid ID value with InvalidTokenCS"
                (InputRefInvalid $ TxOutInvalidEntity $ TxOutInvalidEntityValue $ TxOutInvalidEntityValueID InvalidTokenCS)
                "InputRefInvalid.TxOutInvalidEntity.TxOutInvalidEntityValue.TxOutInvalidEntityValueID.InvalidTokenCS"
            , inputRefTest "Having invalid ID value with InvalidTokenTN"
                (InputRefInvalid $ TxOutInvalidEntity $ TxOutInvalidEntityValue $ TxOutInvalidEntityValueID InvalidTokenTN)
                "InputRefInvalid.TxOutInvalidEntity.TxOutInvalidEntityValue.TxOutInvalidEntityValueID.InvalidTokenTN"
            , inputRefTest "Having invalid ID value with InvalidTokenAmount (Zero)"
                (InputRefInvalid $ TxOutInvalidEntity $ TxOutInvalidEntityValue $ TxOutInvalidEntityValueID (InvalidTokenAmount InvalidValueZero))
                "InputRefInvalid.TxOutInvalidEntity.TxOutInvalidEntityValue.TxOutInvalidEntityValueID.InvalidTokenAmount.InvalidValueZero"
            , inputRefTest "Having invalid ID value with InvalidTokenAmount (Less)"
                (InputRefInvalid $ TxOutInvalidEntity $ TxOutInvalidEntityValue $ TxOutInvalidEntityValueID (InvalidTokenAmount InvalidValueLess))
                "InputRefInvalid.TxOutInvalidEntity.TxOutInvalidEntityValue.TxOutInvalidEntityValueID.InvalidTokenAmount.InvalidValueLess"
            , inputRefTest "Having invalid ID value with InvalidTokenAmount (More)"
                (InputRefInvalid $ TxOutInvalidEntity $ TxOutInvalidEntityValue $ TxOutInvalidEntityValueID (InvalidTokenAmount InvalidValueMore))
                "InputRefInvalid.TxOutInvalidEntity.TxOutInvalidEntityValue.TxOutInvalidEntityValueID.InvalidTokenAmount.InvalidValueMore"
            ]

        -- ADA tests
        adaTests =
            [ inputRefTest "Having invalid ADA value with InvalidTokenAmount (Zero)"
                (InputRefInvalid $ TxOutInvalidEntity $ TxOutInvalidEntityValue $ TxOutInvalidEntityValueADA InvalidValueZero)
                "InputRefInvalid.TxOutInvalidEntity.TxOutInvalidEntityValue.TxOutInvalidEntityValueADA.InvalidValueZero"
            , inputRefTest "Having invalid ADA value with InvalidTokenAmount (Less)"
                (InputRefInvalid $ TxOutInvalidEntity $ TxOutInvalidEntityValue $ TxOutInvalidEntityValueADA InvalidValueLess)
                "InputRefInvalid.TxOutInvalidEntity.TxOutInvalidEntityValue.TxOutInvalidEntityValueADA.InvalidValueLess"
            , inputRefTest "Having invalid ADA value with InvalidTokenAmount (More)"
                (InputRefInvalid $ TxOutInvalidEntity $ TxOutInvalidEntityValue $ TxOutInvalidEntityValueADA InvalidValueMore)
                "InputRefInvalid.TxOutInvalidEntity.TxOutInvalidEntityValue.TxOutInvalidEntityValueADA.InvalidValueMore"
            ]

        -- Other Token tests
        otherTokenTests =
            [ inputRefTest ("Having invalid " ++ tokenName ++ " value with " ++ desc)
                (InputRefInvalid $ TxOutInvalidEntity $ TxOutInvalidEntityValue $
                    TxOutInvalidEntityValueOtherTokens [TokenTestOptions someCS someTN invalidOption])
                ("InputRefInvalid.TxOutInvalidEntity.TxOutInvalidEntityValue.TxOutInvalidEntityValueOtherTokens" ++ "." ++  tokenName ++ "." ++ configDesc)
            | InvalidValueOtherToken tokenName someCS someTN <- otherTokensConfig
            , (desc, invalidOption, configDesc) <-
                [ ("InvalidTokenCS", InvalidTokenCS, "InvalidTokenCS")
                , ("InvalidTokenTN", InvalidTokenTN, "InvalidTokenTN")
                , ("InvalidTokenAmount (Zero)", InvalidTokenAmount InvalidValueZero, "InvalidTokenAmount.InvalidValueZero")
                , ("InvalidTokenAmount (Less)", InvalidTokenAmount InvalidValueLess, "InvalidTokenAmount.InvalidValueLess")
                , ("InvalidTokenAmount (More)", InvalidTokenAmount InvalidValueMore, "InvalidTokenAmount.InvalidValueMore")
                ]
            ]

        -- Datum tests
        datumTests =
            [ inputRefTest "Using an invalid datum with InvalidEntityDatumData"
                (InputRefInvalid $ TxOutInvalidEntity $ TxOutInvalidEntityDatum InvalidEntityDatumData)
                "InputRefInvalid.TxOutInvalidEntity.TxOutInvalidEntityDatum.InvalidEntityDatumData"
            , inputRefTest "Using an invalid datum with InvalidEntityDatumNonExist"
                (InputRefInvalid $ TxOutInvalidEntity $ TxOutInvalidEntityDatum InvalidEntityDatumNonExist)
                "InputRefInvalid.TxOutInvalidEntity.TxOutInvalidEntityDatum.InvalidEntityDatumNonExist"
            , inputRefTest "Using an invalid datum with InvalidEntityDatumType"
                (InputRefInvalid $ TxOutInvalidEntity $ TxOutInvalidEntityDatum InvalidEntityDatumType)
                "InputRefInvalid.TxOutInvalidEntity.TxOutInvalidEntityDatum.InvalidEntityDatumType"
            ]

        -- Address test
        addressTest =
            [ inputRefTest "Having invalid address"
                (InputRefInvalid $ TxOutInvalidEntity TxOutInvalidEntityAddress)
                "InputRefInvalid.TxOutInvalidEntity.TxOutInvalidEntityAddress"
            ]

   in
        Tasty.testGroup
            (pretty entity ++ " Input Ref Tests")
            (concat $ basicTests ++ valueTests ++ adaTests ++ otherTokenTests ++ datumTests ++ addressTest)

----------------------------------------------------------------------------------------

inputs_Tests_Gen :: TestParams -> RedeemerLog -> String -> TxSpecs-> RedeemerTestConfigTree -> TestCaseParams -> Tasty.TestTree
inputs_Tests_Gen tp selectedRedeemer txName txSpecs redeemerTestConfigTree defaultTestCaseParams =
    let
        tests =
            [input_Tests_Gen tp selectedRedeemer txName txSpecs redeemerTestConfigTree defaultTestCaseParams (P.fromIntegral index) | index <- [0.. P.length (tsInputs txSpecs) - 1]]
    in
        Tasty.testGroup
            "Inputs Consume Tests"
            tests

---------------------------

input_Tests_Gen :: TestParams
                   -> RedeemerLog
                   -> String
                   -> TxSpecs
                   -> RedeemerTestConfigTree
                   -> TestCaseParams
                   -> Integer
                   -> Tasty.TestTree
input_Tests_Gen tp selectedRedeemer txName txSpecs redeemerTestConfigTree defaultTestCaseParams indexEntity =
    let
        ----------------------
        inputsTestConfigTree = tctInputs redeemerTestConfigTree
        entityTestConfigTree = inputsTestConfigTree !! fromIntegral indexEntity
        ----------------------
        (entity, _, _) = tsInputs txSpecs !! fromIntegral indexEntity
        otherTokensConfig = testInvalidValueOtherTokens $ getEntityTestConfig tp entity
        ----------------------
        entityName = pretty entity
        ----------------------
        inputTest :: String
             -> InputOptions
             -> String
             -> [Tasty.TestTree]
        inputTest desc inputOption configPath =
            case generateTestCase
                tp
                selectedRedeemer
                (P.const txSpecs)
                []
                defaultTestCaseParams
                entityTestConfigTree
                (desc ++ " for " ++ entityName ++ " input")
                (setTcInputAtIndex indexEntity inputOption)
                configPath
                (txName ++ ", " ++ getRedeemerScriptNameFromLog selectedRedeemer ++ ", " ++ getRedeemerNameFromLog selectedRedeemer ++ ", Inputs, " ++ entityName ++ ".")
                of
                    Nothing -> []
                    Just x  -> [x]
        ----------------------
        -- Generate tests for invalid none and more
        basicTests =
            [
                inputTest "Having valid" InputValid "InputValid",
                inputTest "None valid" (InputInvalid $ InputInvalidTxOut TxOutInvalidNone) "InputInvalid.InputInvalidTxOut.TxOutInvalidNone",
                inputTest "Having more than one" (InputInvalid $ InputInvalidTxOut TxOutInvalidMore) "InputInvalid.InputInvalidTxOut.TxOutInvalidMore"
            ]

        -- Value tests
        valueTests =
            [ inputTest "Having invalid ID value with InvalidTokenCS"
                (InputInvalid $ InputInvalidTxOut $ TxOutInvalidEntity $ TxOutInvalidEntityValue $ TxOutInvalidEntityValueID InvalidTokenCS)
                "InputInvalid.InputInvalidTxOut.TxOutInvalidEntity.TxOutInvalidEntityValue.TxOutInvalidEntityValueID.InvalidTokenCS"
            , inputTest "Having invalid ID value with InvalidTokenTN"
                (InputInvalid $ InputInvalidTxOut $ TxOutInvalidEntity $ TxOutInvalidEntityValue $ TxOutInvalidEntityValueID InvalidTokenTN)
                "InputInvalid.InputInvalidTxOut.TxOutInvalidEntity.TxOutInvalidEntityValue.TxOutInvalidEntityValueID.InvalidTokenTN"
            , inputTest "Having invalid ID value with InvalidTokenAmount (Zero)"
                (InputInvalid $ InputInvalidTxOut $ TxOutInvalidEntity $ TxOutInvalidEntityValue $ TxOutInvalidEntityValueID (InvalidTokenAmount InvalidValueZero))
                "InputInvalid.InputInvalidTxOut.TxOutInvalidEntity.TxOutInvalidEntityValue.TxOutInvalidEntityValueID.InvalidTokenAmount.InvalidValueZero"
            , inputTest "Having invalid ID value with InvalidTokenAmount (Less)"
                (InputInvalid $ InputInvalidTxOut $ TxOutInvalidEntity $ TxOutInvalidEntityValue $ TxOutInvalidEntityValueID (InvalidTokenAmount InvalidValueLess))
                "InputInvalid.InputInvalidTxOut.TxOutInvalidEntity.TxOutInvalidEntityValue.TxOutInvalidEntityValueID.InvalidTokenAmount.InvalidValueLess"
            , inputTest "Having invalid ID value with InvalidTokenAmount (More)"
                (InputInvalid $ InputInvalidTxOut $ TxOutInvalidEntity $ TxOutInvalidEntityValue $ TxOutInvalidEntityValueID (InvalidTokenAmount InvalidValueMore))
                "InputInvalid.InputInvalidTxOut.TxOutInvalidEntity.TxOutInvalidEntityValue.TxOutInvalidEntityValueID.InvalidTokenAmount.InvalidValueMore"
            ]

        -- ADA tests
        adaTests =
            [ inputTest "Having invalid ADA value with InvalidTokenAmount (Zero)"
                (InputInvalid $ InputInvalidTxOut $ TxOutInvalidEntity $ TxOutInvalidEntityValue $ TxOutInvalidEntityValueADA InvalidValueZero)
                "InputInvalid.InputInvalidTxOut.TxOutInvalidEntity.TxOutInvalidEntityValue.TxOutInvalidEntityValueADA.InvalidValueZero"
            , inputTest "Having invalid ADA value with InvalidTokenAmount (Less)"
                (InputInvalid $ InputInvalidTxOut $ TxOutInvalidEntity $ TxOutInvalidEntityValue $ TxOutInvalidEntityValueADA InvalidValueLess)
                "InputInvalid.InputInvalidTxOut.TxOutInvalidEntity.TxOutInvalidEntityValue.TxOutInvalidEntityValueADA.InvalidValueLess"
            , inputTest "Having invalid ADA value with InvalidTokenAmount (More)"
                (InputInvalid $ InputInvalidTxOut $ TxOutInvalidEntity $ TxOutInvalidEntityValue $ TxOutInvalidEntityValueADA InvalidValueMore)
                "InputInvalid.InputInvalidTxOut.TxOutInvalidEntity.TxOutInvalidEntityValue.TxOutInvalidEntityValueADA.InvalidValueMore"
            ]

        -- Other Token tests
        otherTokenTests =
            [ inputTest ("Having invalid " ++ tokenName ++ " value with " ++ desc)
                (InputInvalid $ InputInvalidTxOut $ TxOutInvalidEntity $ TxOutInvalidEntityValue $
                    TxOutInvalidEntityValueOtherTokens [TokenTestOptions someCS someTN invalidOption])
                ("InputInvalid.InputInvalidTxOut.TxOutInvalidEntity.TxOutInvalidEntityValue.TxOutInvalidEntityValueOtherTokens" ++ "." ++  tokenName ++ "." ++ configDesc)
            | InvalidValueOtherToken tokenName someCS someTN <- otherTokensConfig
            , (desc, invalidOption, configDesc) <-
                [ ("InvalidTokenCS", InvalidTokenCS, "InvalidTokenCS")
                , ("InvalidTokenTN", InvalidTokenTN, "InvalidTokenTN")
                , ("InvalidTokenAmount (Zero)", InvalidTokenAmount InvalidValueZero, "InvalidTokenAmount.InvalidValueZero")
                , ("InvalidTokenAmount (Less)", InvalidTokenAmount InvalidValueLess, "InvalidTokenAmount.InvalidValueLess")
                , ("InvalidTokenAmount (More)", InvalidTokenAmount InvalidValueMore, "InvalidTokenAmount.InvalidValueMore")
                ]
            ]

        -- Datum tests
        datumTests =
            [ inputTest "Using an invalid datum with InvalidEntityDatumData"
                (InputInvalid $ InputInvalidTxOut $ TxOutInvalidEntity $ TxOutInvalidEntityDatum InvalidEntityDatumData)
                "InputInvalid.InputInvalidTxOut.TxOutInvalidEntity.TxOutInvalidEntityDatum.InvalidEntityDatumData"
            , inputTest "Using an invalid datum with InvalidEntityDatumNonExist"
                (InputInvalid $ InputInvalidTxOut $ TxOutInvalidEntity $ TxOutInvalidEntityDatum InvalidEntityDatumNonExist)
                "InputInvalid.InputInvalidTxOut.TxOutInvalidEntity.TxOutInvalidEntityDatum.InvalidEntityDatumNonExist"
            , inputTest "Using an invalid datum with InvalidEntityDatumType"
                (InputInvalid $ InputInvalidTxOut $ TxOutInvalidEntity $ TxOutInvalidEntityDatum InvalidEntityDatumType)
                "InputInvalid.InputInvalidTxOut.TxOutInvalidEntity.TxOutInvalidEntityDatum.InvalidEntityDatumType"
            ]

        -- Address test
        addressTest =
            [ inputTest "Having invalid address"
                (InputInvalid $ InputInvalidTxOut $ TxOutInvalidEntity TxOutInvalidEntityAddress)
                "InputInvalid.InputInvalidTxOut.TxOutInvalidEntity.TxOutInvalidEntityAddress"
            ]

        redeemerTest =
            [ inputTest "Using an invalid redeemer with InvalidRedeemerData"
                (InputInvalid $ InputInvalidRedeemer InvalidRedeemerData)
                "InputInvalid.InputInvalidRedeemer.InvalidRedeemerData"
            , inputTest "Using an invalid redeemer with InvalidRedeemerNonExist"
                (InputInvalid $ InputInvalidRedeemer InvalidRedeemerNonExist)
                "InputInvalid.InputInvalidRedeemer.InvalidRedeemerNonExist"
            , inputTest "Using an invalid redeemer with InvalidRedeemerType"
                (InputInvalid $ InputInvalidRedeemer InvalidRedeemerType)
                "InputInvalid.InputInvalidRedeemer.InvalidRedeemerType"
            ]

   in
        Tasty.testGroup
            (pretty entity ++ " Input Tests")
            (concat $ basicTests ++ valueTests ++ adaTests ++ otherTokenTests ++ datumTests ++ addressTest ++ redeemerTest)

----------------------------------------------------------------------------------------

outputs_Tests_Gen :: TestParams -> RedeemerLog -> String -> TxSpecs -> RedeemerTestConfigTree -> TestCaseParams -> Tasty.TestTree
outputs_Tests_Gen tp selectedRedeemer txName txSpecs redeemerTestConfigTree defaultTestCaseParams =
    let
        tests =
            [output_Tests_Gen tp selectedRedeemer txName txSpecs redeemerTestConfigTree defaultTestCaseParams (P.fromIntegral index) | index <- [0.. P.length (tsOutputs txSpecs) -1]]
    in
        Tasty.testGroup
            "Outputs Tests"
            tests

---------------------------

output_Tests_Gen :: TestParams
                   -> RedeemerLog
                   -> String
                   -> TxSpecs
                   -> RedeemerTestConfigTree
                   -> TestCaseParams
                   -> Integer
                   -> Tasty.TestTree
output_Tests_Gen tp selectedRedeemer txName txSpecs redeemerTestConfigTree defaultTestCaseParams indexEntity =
    let
        ----------------------
        outputsTestConfigTree = tctOutputs redeemerTestConfigTree
        entityTestConfigTree = outputsTestConfigTree !! fromIntegral indexEntity
        ----------------------
        entity = fst $ tsOutputs txSpecs !! fromIntegral indexEntity
        otherTokensConfig = testInvalidValueOtherTokens $ getEntityTestConfig tp entity
        ----------------------
        entityName = pretty entity
        ----------------------
        outputTest :: String
             -> OutputOptions
             -> String
             -> [Tasty.TestTree]
        outputTest desc outputOption configPath =
            case generateTestCase
                tp
                selectedRedeemer
                (P.const txSpecs)
                []
                defaultTestCaseParams
                entityTestConfigTree
                (desc ++ " for " ++ entityName ++ " output")
                (setTcOutputAtIndex indexEntity outputOption)
                configPath
                (txName ++ ", " ++ getRedeemerScriptNameFromLog selectedRedeemer ++ ", " ++ getRedeemerNameFromLog selectedRedeemer ++ ", Outputs, " ++ entityName ++ ".")
                of
                    Nothing -> []
                    Just x  -> [x]
        ----------------------
        -- Generate tests for invalid none and more
        basicTests =
            [
                outputTest "Having valid" OutputValid  "OutputValid",
                outputTest "None valid" (OutputInvalid TxOutInvalidNone) "OutputInvalid.TxOutInvalidNone",
                outputTest "Having more than one" (OutputInvalid TxOutInvalidMore) "OutputInvalid.TxOutInvalidMore"
            ]

        -- Value tests
        valueTests =
            [ outputTest "Having invalid ID value with InvalidTokenCS"
                (OutputInvalid $ TxOutInvalidEntity $ TxOutInvalidEntityValue $ TxOutInvalidEntityValueID InvalidTokenCS)
                "OutputInvalid.TxOutInvalidEntity.TxOutInvalidEntityValue.TxOutInvalidEntityValueID.InvalidTokenCS"
            , outputTest "Having invalid ID value with InvalidTokenTN"
                (OutputInvalid $ TxOutInvalidEntity $ TxOutInvalidEntityValue $ TxOutInvalidEntityValueID InvalidTokenTN)
                "OutputInvalid.TxOutInvalidEntity.TxOutInvalidEntityValue.TxOutInvalidEntityValueID.InvalidTokenTN"
            , outputTest "Having invalid ID value with InvalidTokenAmount (Zero)"
                (OutputInvalid $ TxOutInvalidEntity $ TxOutInvalidEntityValue $ TxOutInvalidEntityValueID (InvalidTokenAmount InvalidValueZero))
                "OutputInvalid.TxOutInvalidEntity.TxOutInvalidEntityValue.TxOutInvalidEntityValueID.InvalidTokenAmount.InvalidValueZero"
            , outputTest "Having invalid ID value with InvalidTokenAmount (Less)"
                (OutputInvalid $ TxOutInvalidEntity $ TxOutInvalidEntityValue $ TxOutInvalidEntityValueID (InvalidTokenAmount InvalidValueLess))
                "OutputInvalid.TxOutInvalidEntity.TxOutInvalidEntityValue.TxOutInvalidEntityValueID.InvalidTokenAmount.InvalidValueLess"
            , outputTest "Having invalid ID value with InvalidTokenAmount (More)"
                (OutputInvalid $ TxOutInvalidEntity $ TxOutInvalidEntityValue $ TxOutInvalidEntityValueID (InvalidTokenAmount InvalidValueMore))
                "OutputInvalid.TxOutInvalidEntity.TxOutInvalidEntityValue.TxOutInvalidEntityValueID.InvalidTokenAmount.InvalidValueMore"
            ]

        -- ADA tests
        adaTests =
            [ outputTest "Having invalid ADA value with InvalidTokenAmount (Zero)"
                (OutputInvalid $ TxOutInvalidEntity $ TxOutInvalidEntityValue $ TxOutInvalidEntityValueADA InvalidValueZero)
                "OutputInvalid.TxOutInvalidEntity.TxOutInvalidEntityValue.TxOutInvalidEntityValueADA.InvalidValueZero"
            , outputTest "Having invalid ADA value with InvalidTokenAmount (Less)"
                (OutputInvalid $ TxOutInvalidEntity $ TxOutInvalidEntityValue $ TxOutInvalidEntityValueADA InvalidValueLess)
                "OutputInvalid.TxOutInvalidEntity.TxOutInvalidEntityValue.TxOutInvalidEntityValueADA.InvalidValueLess"
            , outputTest "Having invalid ADA value with InvalidTokenAmount (More)"
                (OutputInvalid $ TxOutInvalidEntity $ TxOutInvalidEntityValue $ TxOutInvalidEntityValueADA InvalidValueMore)
                "OutputInvalid.TxOutInvalidEntity.TxOutInvalidEntityValue.TxOutInvalidEntityValueADA.InvalidValueMore"
            ]

        -- Other Token tests
        otherTokenTests =
            [ outputTest ("Having invalid " ++ tokenName ++ " value with " ++ desc)
                (OutputInvalid $ TxOutInvalidEntity $ TxOutInvalidEntityValue $
                    TxOutInvalidEntityValueOtherTokens [TokenTestOptions someCS someTN invalidOption])
                ("OutputInvalid.TxOutInvalidEntity.TxOutInvalidEntityValue.TxOutInvalidEntityValueOtherTokens" ++ "." ++  tokenName ++ "." ++ configDesc)
            | InvalidValueOtherToken tokenName someCS someTN <- otherTokensConfig
            , (desc, invalidOption, configDesc) <-
                [ ("InvalidTokenCS", InvalidTokenCS, "InvalidTokenCS")
                , ("InvalidTokenTN", InvalidTokenTN, "InvalidTokenTN")
                , ("InvalidTokenAmount (Zero)", InvalidTokenAmount InvalidValueZero, "InvalidTokenAmount.InvalidValueZero")
                , ("InvalidTokenAmount (Less)", InvalidTokenAmount InvalidValueLess, "InvalidTokenAmount.InvalidValueLess")
                , ("InvalidTokenAmount (More)", InvalidTokenAmount InvalidValueMore, "InvalidTokenAmount.InvalidValueMore")
                ]
            ]

        -- Datum tests
        datumTests =
            [ outputTest "Using an invalid datum with InvalidEntityDatumData"
                (OutputInvalid $ TxOutInvalidEntity $ TxOutInvalidEntityDatum InvalidEntityDatumData)
                "OutputInvalid.TxOutInvalidEntity.TxOutInvalidEntityDatum.InvalidEntityDatumData"
            , outputTest "Using an invalid datum with InvalidEntityDatumNonExist"
                (OutputInvalid $ TxOutInvalidEntity $ TxOutInvalidEntityDatum InvalidEntityDatumNonExist)
                "OutputInvalid.TxOutInvalidEntity.TxOutInvalidEntityDatum.InvalidEntityDatumNonExist"
            , outputTest "Using an invalid datum with InvalidEntityDatumType"
                (OutputInvalid $ TxOutInvalidEntity $ TxOutInvalidEntityDatum InvalidEntityDatumType)
                "OutputInvalid.TxOutInvalidEntity.TxOutInvalidEntityDatum.InvalidEntityDatumType"
            ]

        -- Address test
        addressTest =
            [ outputTest "Having invalid address"
                (OutputInvalid $ TxOutInvalidEntity TxOutInvalidEntityAddress)
                "OutputInvalid.TxOutInvalidEntity.TxOutInvalidEntityAddress"
            ]

   in
        Tasty.testGroup
            (pretty entity ++ " Outputs Tests")
            (concat $ basicTests ++ valueTests ++ adaTests ++ otherTokenTests ++ datumTests ++ addressTest)

----------------------------------------------------------------------------------------

mints_Tests_Gen ::  TestParams -> RedeemerLog -> String -> TxSpecs -> RedeemerTestConfigTree -> TestCaseParams -> Tasty.TestTree
mints_Tests_Gen tp selectedRedeemer  =
        mints_base_Tests_Gen tp selectedRedeemer "mint"

---------------------------

burns_Tests_Gen :: TestParams -> RedeemerLog -> String -> TxSpecs -> RedeemerTestConfigTree -> TestCaseParams -> Tasty.TestTree
burns_Tests_Gen tp selectedRedeemer  =
        mints_base_Tests_Gen tp selectedRedeemer "burn"

---------------------------

mints_base_Tests_Gen :: TestParams -> RedeemerLog -> String -> String -> TxSpecs -> RedeemerTestConfigTree -> TestCaseParams -> Tasty.TestTree
mints_base_Tests_Gen tp selectedRedeemer prefixMintingStr txName txSpecs redeemerTestConfigTree defaultTestCaseParams =
    let
        tests =
            [mint_base_Tests_Gen tp selectedRedeemer prefixMintingStr txName txSpecs redeemerTestConfigTree defaultTestCaseParams (P.fromIntegral index) | index <- [0.. P.length (tsMints txSpecs) -1]]
    in
        Tasty.testGroup
            (capitalizeFirst prefixMintingStr ++ "ing Tests")
            tests

---------------------------

mint_base_Tests_Gen :: TestParams
               -> RedeemerLog
               -> String
               -> String
               -> TxSpecs
               -> RedeemerTestConfigTree
               -> TestCaseParams
               -> Integer
               -> Tasty.TestTree
mint_base_Tests_Gen tp selectedRedeemer prefixMintingStr txName txSpecs redeemerTestConfigTree defaultTestCaseParams indexEntity =
    let
        ----------------------
        (tokenMint, _, _) = tsMints txSpecs !! fromIntegral indexEntity
        ----------------------
        mintsTestConfigTree = tctMints redeemerTestConfigTree
        entityTestConfigTree = mintsTestConfigTree !! fromIntegral indexEntity
        ----------------------
        tokenMintName = pretty tokenMint
        ----------------------
        mintTest :: String
                 -> MintOptions
                 -> String
                 -> [Tasty.TestTree]
        mintTest desc mintOption configPath =
            case generateTestCase
                tp
                selectedRedeemer
                (P.const txSpecs)
                []
                defaultTestCaseParams
                entityTestConfigTree
                (desc ++ " for " ++ tokenMintName ++ " " ++ prefixMintingStr ++ "ing")
                (setTcMintsAtIndex indexEntity mintOption)
                configPath
                (txName ++ ", " ++ getRedeemerScriptNameFromLog selectedRedeemer ++ ", " ++ getRedeemerNameFromLog selectedRedeemer ++ ", Mints, " ++ tokenMintName ++ ".")
                of
                    Nothing -> []
                    Just x  -> [x]
        ----------------------
        -- Generate tests for valid and invalid minting
        basicTests =
            [
                mintTest "Having valid" MintValid "MintValid"
            ]

        -- Redeemer tests
        redeemerTests =
            [ mintTest "Using an invalid redeemer with InvalidRedeemerData"
                (MintInvalid $ MintInvalidRedeemer InvalidRedeemerData)
                "MintInvalid.MintInvalidRedeemer.InvalidRedeemerData"
            , mintTest "Using an invalid redeemer with InvalidRedeemerNonExist"
                (MintInvalid $ MintInvalidRedeemer InvalidRedeemerNonExist)
                "MintInvalid.MintInvalidRedeemer.InvalidRedeemerNonExist"
            , mintTest "Using an invalid redeemer with InvalidRedeemerType"
                (MintInvalid $ MintInvalidRedeemer InvalidRedeemerType)
                "MintInvalid.MintInvalidRedeemer.InvalidRedeemerType"
            ]

        -- Value tests
        valueTests =
            [ mintTest "Using invalid Currency Symbol"
                (MintInvalid $ MintInvalidValue InvalidTokenCS)
                "MintInvalid.MintInvalidValue.InvalidTokenCS"
            , mintTest "Using invalid Token Name"
                (MintInvalid $ MintInvalidValue InvalidTokenTN)
                "MintInvalid.MintInvalidValue.InvalidTokenTN"
            , mintTest "Using zero token amount"
                (MintInvalid $ MintInvalidValue $ InvalidTokenAmount InvalidValueZero)
                "MintInvalid.MintInvalidValue.InvalidTokenAmount.InvalidValueZero"
            , mintTest "Using less token amount"
                (MintInvalid $ MintInvalidValue $ InvalidTokenAmount InvalidValueLess)
                "MintInvalid.MintInvalidValue.InvalidTokenAmount.InvalidValueLess"
            , mintTest "Using more token amount"
                (MintInvalid $ MintInvalidValue $ InvalidTokenAmount InvalidValueMore)
                "MintInvalid.MintInvalidValue.InvalidTokenAmount.InvalidValueMore"
            ]

    in
        Tasty.testGroup
            (tokenMintName ++ " Minting Tests")
            (concat $ basicTests ++ redeemerTests ++ valueTests)

----------------------------------------------------------------------------------------

signatures_Tests_Gen :: TestParams -> RedeemerLog -> String -> TxSpecs -> RedeemerTestConfigTree -> TestCaseParams -> Tasty.TestTree
signatures_Tests_Gen tp selectedRedeemer txName txSpecs redeemerTestConfigTree defaultTestCaseParams =
    let
        ----------------------
        signatureTestConfigTree = tctSignatures redeemerTestConfigTree
        ----------------------
        signatureTest :: String
                      -> SignaturesOptions
                      -> String
                      -> [Tasty.TestTree]
        signatureTest desc signaturesOption configPath =
            case generateTestCase
                tp
                selectedRedeemer
                (P.const txSpecs)
                []
                defaultTestCaseParams
                signatureTestConfigTree
                desc
                (setTcSignatures signaturesOption)
                configPath
                (txName ++ ", " ++ getRedeemerScriptNameFromLog selectedRedeemer ++ ", " ++ getRedeemerNameFromLog selectedRedeemer ++ ", Signatures, ")
                of
                    Nothing -> []
                    Just x  -> [x]
        ----------------------
        -- Generate tests for valid, none, and other signature options
        signatureTests =
            [ signatureTest "Using valid signatures" SignatureValid "SignatureValid"
            , signatureTest "Using no signatures" SignatureNone "SignatureNone"
            , signatureTest "Using other invalid signatures" SignatureOther "SignatureOther"
            ]
        ----------------------
        tests = if DataMaybe.isJust $ tsUseSignatures txSpecs then concat signatureTests else []
    in
        Tasty.testGroup "Signatures Tests" tests

----------------------------------------------------------------------------------------

validyRange_Tests_Gen :: TestParams -> RedeemerLog -> String -> TxSpecs -> RedeemerTestConfigTree -> TestCaseParams -> Tasty.TestTree
validyRange_Tests_Gen tp selectedRedeemer txName txSpecs redeemerTestConfigTree defaultTestCaseParams =
    let
        ----------------------
        validyRangeTestConfigTree = tctValidyRange redeemerTestConfigTree
        ----------------------
        validyRangeTest :: String
             -> ValidityRangeOptions
             -> String
             -> [Tasty.TestTree]
        validyRangeTest desc validyRangeOption configPath =
            case generateTestCase
                tp
                selectedRedeemer
                (P.const txSpecs)
                []
                defaultTestCaseParams
                validyRangeTestConfigTree
                desc
                (setTcValidityRange validyRangeOption)
                configPath
                (txName ++ ", " ++ getRedeemerScriptNameFromLog selectedRedeemer ++ ", " ++ getRedeemerNameFromLog selectedRedeemer ++ ", ValidityRange, ")
                of
                    Nothing -> []
                    Just x  -> [x]
        ----------------------
        -- Generate tests for valid, invalid, and none range options
        validityRangeTests =
            [ validyRangeTest "Using valid range" RangeValid "RangeValid"
            , validyRangeTest "Using invalid range" RangeInvalid "RangeInvalid"
            , validyRangeTest "Using no specific range" RangeNone "RangeNone"
            ]
        ----------------------
        tests = if DataMaybe.isJust $ tsUseValidityRange txSpecs then concat validityRangeTests else []
    in
        Tasty.testGroup "Validy Range Tests" tests

----------------------------------------------------------------------------------------

extras_Tests_Gen :: TestParams -> RedeemerLog -> String -> TxSpecs -> RedeemerTestConfigTree -> TestCaseParams -> Tasty.TestTree
extras_Tests_Gen tp selectedRedeemer txName txSpecs redeemerTestConfigTree defaultTestCaseParams =
    let
        extrasTestConfigTree = tctExtras redeemerTestConfigTree

        extraTest :: String -> [Tasty.TestTree]
        extraTest extraName =
            case generateTestCase
                tp
                selectedRedeemer
                (P.const txSpecs)
                []
                defaultTestCaseParams
                extrasTestConfigTree
                ("Having " ++ extraName)
                (setTcExtras [(extraName, True)])
                extraName
                (txName ++ ", " ++ getRedeemerScriptNameFromLog selectedRedeemer ++ ", " ++ getRedeemerNameFromLog selectedRedeemer ++ ", Extras, ")
                of
                    Nothing -> []
                    Just x  -> [x]

        tests = concatMap extraTest (fst <$> filter (not . snd) (tsExtras txSpecs))
    in
        Tasty.testGroup "Specific Tests" tests

---------------------------

extras_Property_Tests_Gen :: GHC.HasCallStack => TestParams -> RedeemerLog -> String -> TxSpecsParametrizable -> TxSpecs -> TxParamGeneratorsList  -> RedeemerTestConfigTree -> TestCaseParams -> Tasty.TestTree
extras_Property_Tests_Gen tp selectedRedeemer txName txSpecs defaultTxSpecs txParamsGenerators_List redeemerTestConfigTree defaultTestCaseParams =
    let
        extrasTestConfigTree = tctExtras redeemerTestConfigTree

        extraTest :: String -> [Tasty.TestTree]
        extraTest extraName =
            case generateTestCase
                tp
                selectedRedeemer
                txSpecs
                txParamsGenerators_List
                defaultTestCaseParams
                extrasTestConfigTree
                ("Having " ++ extraName)
                (setTcExtras [(extraName, True)])
                extraName
                (txName ++ ", " ++ getRedeemerScriptNameFromLog selectedRedeemer ++ ", " ++ getRedeemerNameFromLog selectedRedeemer ++ ", Extras, ")
                of
                    Nothing -> []
                    Just x  -> [x]
        tests = concatMap extraTest (fst <$> filter snd (tsExtras defaultTxSpecs))
    in
        Tasty.testGroup "Property Based Tests" tests

----------------------------------------------------------------------------------------

adminTokens_Tests_Gen ::
    TestParams ->
    String ->  -- txName
    RedeemerLog ->  -- selectedRedeemer
    (TestParams -> TxSpecs) ->  -- txSpecs
    LedgerApiV2.Redeemer ->  -- mkEmergencyRedeemer
    T.CS ->  -- tokenAdminPolicy_CS
    T.CS ->  -- tokenEmergencyAdminPolicy_CS
    T.TN ->  -- protocolTokenAdmin_TN
    T.TN ->  -- protocolTokenEmergencyAdmin_TN
    Bool ->  -- useAdminToken
    Bool ->  -- useEmergencyToken
    Tasty.TestTree
adminTokens_Tests_Gen tp txName selectedRedeemer txSpecsFn mkEmergencyRedeemer tokenAdminPolicy_CS tokenEmergencyAdminPolicy_CS protocolTokenAdmin_TN protocolTokenEmergencyAdmin_TN useAdminToken useEmergencyToken =
    let
        -------------------
        txSpecs = txSpecsFn tp
        defaultTestCaseParams = generateTestCaseParams txSpecs
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        -------------------
        tokenAdmin_Value = LedgerValue.singleton tokenAdminPolicy_CS protocolTokenAdmin_TN 1
        tokenEmergency_Value = LedgerValue.singleton tokenEmergencyAdminPolicy_CS protocolTokenEmergencyAdmin_TN 1
        -------------------
        outputToWalletWithAdminTokenUTxO = LedgerApiV2.TxOut
            { LedgerApiV2.txOutAddress = LedgerAddress.Address
                { LedgerApiV2.addressCredential = LedgerApiV2.PubKeyCredential $ LedgerApiV2.PubKeyHash "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e"
                , LedgerApiV2.addressStakingCredential = Nothing
                }
            , LedgerApiV2.txOutDatum = LedgerApiV2.NoOutputDatum
            , LedgerApiV2.txOutValue = LedgerAda.lovelaceValueOf 5000000 <> tokenAdmin_Value
            , LedgerApiV2.txOutReferenceScript = Nothing
            }

        outputToWalletWithEmergencyTokenUTxO = LedgerApiV2.TxOut
            { LedgerApiV2.txOutAddress = LedgerAddress.Address
                { LedgerApiV2.addressCredential = LedgerApiV2.PubKeyCredential $ LedgerApiV2.PubKeyHash "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e"
                , LedgerApiV2.addressStakingCredential = Nothing
                }
            , LedgerApiV2.txOutDatum = LedgerApiV2.NoOutputDatum
            , LedgerApiV2.txOutValue = LedgerAda.lovelaceValueOf 5000000 <> tokenEmergency_Value
            , LedgerApiV2.txOutReferenceScript = Nothing
            }

        -- Test cases for scripts using only admin token
        testCasesAdminToken =
            [ ("Valid datum, admin redeemer, no signature, no admin token - must fail", False, False, False, False, False, False)
            , ("Valid datum, admin redeemer, no signature, admin token - must succeed", False, False, False, True, False, True)
            , ("Valid datum, admin redeemer, signature, no admin token - must succeed", False, False, True, False, False, True)
            , ("Valid datum, admin redeemer, signature, admin token - must succeed", False, False, True, True, False, True)
            , ("Invalid datum, admin redeemer, no signature, no admin token - must fail", True, False, False, False, False, False)
            , ("Invalid datum, admin redeemer, no signature, admin token - must fail", True, False, False, True, False, False)
            , ("Invalid datum, admin redeemer, signature, no admin token - must fail", True, False, True, False, False, False)
            , ("Invalid datum, admin redeemer, signature, admin token - must fail", True, False, True, True, False, False)
            ]

        -- Test cases for scripts using only emergency token
        testCasesEmergencyToken =
            [ ("Valid datum, admin redeemer, no signature, no emergency token - must fail", False, False, False, False, False, False)
            , ("Valid datum, admin redeemer, no signature, emergency token - must fail", False, False, False, False, True, False)
            , ("Valid datum, admin redeemer, signature, no emergency token - must succeed", False, False, True, False, False, True)
            , ("Valid datum, admin redeemer, signature, emergency token - must succeed", False, False, True, False, True, True)
            , ("Valid datum, emergency redeemer, no signature, no emergency token - must fail", False, True, False, False, False, False)
            , ("Valid datum, emergency redeemer, no signature, emergency token - must succeed", False, True, False, False, True, True)
            , ("Valid datum, emergency redeemer, signature, no emergency token - must fail", False, True, True, False, False, False)
            , ("Valid datum, emergency redeemer, signature, emergency token - must succeed", False, True, True, False, True, True)
            , ("Invalid datum, admin redeemer, no signature, no emergency token - must fail", True, False, False, False, False, False)
            , ("Invalid datum, admin redeemer, no signature, emergency token - must fail", True, False, False, False, True, False)
            , ("Invalid datum, admin redeemer, signature, no emergency token - must fail", True, False, True, False, False, False)
            , ("Invalid datum, admin redeemer, signature, emergency token - must fail", True, False, True, False, True, False)
            , ("Invalid datum, emergency redeemer, no signature, no emergency token - must fail", True, True, False, False, False, False)
            , ("Invalid datum, emergency redeemer, no signature, emergency token - must succeed", True, True, False, False, True, True)
            , ("Invalid datum, emergency redeemer, signature, no emergency token - must fail", True, True, True, False, False, False)
            , ("Invalid datum, emergency redeemer, signature, emergency token - must succeed", True, True, True, False, True, True)
            ]

        -- Test cases for scripts using both admin and emergency tokens
        --  invalidDatum, isEmergencyRedeemer, hasSignature, hasAdminToken, hasEmergencyToken, expectedOutcome
        testCasesBothTokens =
            [ ("Valid datum, admin redeemer, no signature, no tokens - must fail", False, False, False, False, False, False)
            , ("Valid datum, admin redeemer, no signature, admin token - must succeed", False, False, False, True, False, True)
            , ("Valid datum, admin redeemer, no signature, emergency token - must fail", False, False, False, False, True, False)
            , ("Valid datum, admin redeemer, no signature, both tokens - must succeed", False, False, False, True, True, True)
            , ("Valid datum, admin redeemer, signature, no tokens - must succeed", False, False, True, False, False, True)
            , ("Valid datum, admin redeemer, signature, admin token - must succeed", False, False, True, True, False, True)
            , ("Valid datum, admin redeemer, signature, emergency token - must succeed", False, False, True, False, True, True)
            , ("Valid datum, admin redeemer, signature, both tokens - must succeed", False, False, True, True, True, True)
            , ("Valid datum, emergency redeemer, no signature, no tokens - must fail", False, True, False, False, False, False)
            , ("Valid datum, emergency redeemer, no signature, admin token - must fail", False, True, False, True, False, False)
            , ("Valid datum, emergency redeemer, no signature, emergency token - must succeed", False, True, False, False, True, True)
            , ("Valid datum, emergency redeemer, no signature, both tokens - must succeed", False, True, False, True, True, True)
            , ("Valid datum, emergency redeemer, signature, no tokens - must fail", False, True, True, False, False, False)
            , ("Valid datum, emergency redeemer, signature, admin token - must fail", False, True, True, True, False, False)
            , ("Valid datum, emergency redeemer, signature, emergency token - must succeed", False, True, True, False, True, True)
            , ("Valid datum, emergency redeemer, signature, both tokens - must succeed", False, True, True, True, True, True)
            , ("Invalid datum, admin redeemer, no signature, no tokens - must fail", True, False, False, False, False, False)
            , ("Invalid datum, admin redeemer, no signature, admin token - must fail", True, False, False, True, False, False)
            , ("Invalid datum, admin redeemer, no signature, emergency token - must fail", True, False, False, False, True, False)
            , ("Invalid datum, admin redeemer, no signature, both tokens - must fail", True, False, False, True, True, False)
            , ("Invalid datum, admin redeemer, signature, no tokens - must fail", True, False, True, False, False, False)
            , ("Invalid datum, admin redeemer, signature, admin token - must fail", True, False, True, True, False, False)
            , ("Invalid datum, admin redeemer, signature, emergency token - must fail", True, False, True, False, True, False)
            , ("Invalid datum, admin redeemer, signature, both tokens - must fail", True, False, True, True, True, False)
            , ("Invalid datum, emergency redeemer, no signature, no tokens - must fail", True, True, False, False, False, False)
            , ("Invalid datum, emergency redeemer, no signature, admin token - must fail", True, True, False, True, False, False)
            , ("Invalid datum, emergency redeemer, no signature, emergency token - must succeed", True, True, False, False, True, True)
            , ("Invalid datum, emergency redeemer, no signature, both tokens - must succeed", True, True, False, True, True, True)
            , ("Invalid datum, emergency redeemer, signature, no tokens - must fail", True, True, True, False, False, False)
            , ("Invalid datum, emergency redeemer, signature, admin token - must fail", True, True, True, True, False, False)
            , ("Invalid datum, emergency redeemer, signature, emergency token - must succeed", True, True, True, False, True, True)
            , ("Invalid datum, emergency redeemer, signature, both tokens - must succeed", True, True, True, True, True, True)
            ]

        -- Update the testCases selection
        testCases = case (useAdminToken, useEmergencyToken) of
            (True, False) -> testCasesAdminToken
            (False, True) -> testCasesEmergencyToken
            (True, True)  -> testCasesBothTokens
            _             -> error "Invalid token configuration"

        -- Update the pattern matching in the test case generation
        in Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
            flip map testCases $ \(testName, invalidDatum, isEmergencyRedeemer, hasSignature, hasAdminToken, hasEmergencyToken, expectedOutcome) ->

            Tasty.testCase testName $ do
                let
                    ------------------------
                    updateInput _ =
                        if invalidDatum
                        then InputInvalid $ InputInvalidTxOut $ TxOutInvalidEntity $ TxOutInvalidEntityDatum InvalidEntityDatumData
                        else InputValid
                    ------------------------
                    updatedTestCaseParams = defaultTestCaseParams
                        { tcSignatures = if hasSignature then SignatureValid else SignatureNone
                        , tcInputs = map updateInput (tcInputs defaultTestCaseParams) -- tiene que tener el mismo tamaño que los defaultTestCaseParams
                        }
                    ------------------------
                    ctx = context_Gen tp txSpecs updatedTestCaseParams
                    ------------------------
                    origInputsTxOuts = LedgerApiV2.txInInfoResolved <$> LedgerApiV2.txInfoInputs (LedgerApiV2.scriptContextTxInfo ctx)
                    origOutputsTxOuts = LedgerApiV2.txInfoOutputs $ LedgerApiV2.scriptContextTxInfo ctx
                    ------------------------
                    additionalOutputs =
                        -- es importante el orden de los tokens, siempre se busca el token en output 0
                        -- por eso si estoy en emergency redeemer, pongo en 0 el emergency token
                        if isEmergencyRedeemer then
                            ([outputToWalletWithEmergencyTokenUTxO | hasEmergencyToken]) ++
                            ([outputToWalletWithAdminTokenUTxO | hasAdminToken])
                        else
                            ([outputToWalletWithAdminTokenUTxO | hasAdminToken]) ++
                            ([outputToWalletWithEmergencyTokenUTxO | hasEmergencyToken])
                    ------------------------
                    ctx' = if isEmergencyRedeemer
                        then ctx
                                |> setInputsAndAddRedeemers (map (\x -> (x, mkEmergencyRedeemer)) origInputsTxOuts)
                                -- es muy importante que los tokens esten en output 0
                                |> setOutputs (additionalOutputs ++ origOutputsTxOuts)
                        else ctx
                                -- es muy importante que los tokens esten en output 0
                                |> setOutputs (additionalOutputs ++ origOutputsTxOuts)
                    ------------------------
                results <- testContextWrapper tp ctx'
                if expectedOutcome then assertResultsContainNoError (Nothing, results) else assertResultsContainError (Nothing, results)

----------------------------------------------------------------------------------------
