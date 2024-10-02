--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module TestUtils.Automatic.TestCaseGenerator where

--------------------------------------------------------------------------------

-- Non-IOG imports
import qualified Data.Text                            as DataText
import qualified GHC.Stack                            as GHC
import           Prelude                              as P hiding ((<>))
import qualified Test.QuickCheck                      as QC
import qualified Test.QuickCheck.Monadic              as QCM
import qualified Test.Tasty                           as Tasty
import qualified Test.Tasty.HUnit                     as Tasty
import qualified Test.Tasty.QuickCheck                as TastyQC
import qualified Text.Regex.TDFA                      as Regex

-- IOG imports

-- Project imports

import           TestUtils.Automatic.ContextGenerator
import           TestUtils.Automatic.ParamsGenerators
import           TestUtils.Automatic.TestConfigTree
import           TestUtils.Automatic.Types
import           TestUtils.HelpersMAYZ
import           TestUtils.TestContext.Asserts
import           TestUtils.TypesMAYZ

----------------------------------------------------------------------------------------

-- Generate a single test case as a QuickCheck property with detailed failure information
generateTestCase :: GHC.HasCallStack => TestParams -> RedeemerLog -> TxSpecsParametrizable -> TxParamGeneratorsList -> TestCaseParams -> ConfigTestTree -> String -> (TestCaseParams -> TestCaseParams) -> String -> String -> Maybe Tasty.TestTree
generateTestCase tp selectedRedeemer txSpecs txParamsGenerators_List defaultTestCaseParams configTestTree testDescription modifyTestParamFn configPath basePath =
    let
        -------------------
        getUpdatedRedeemerLog :: RedeemerLog -> ErrorSource -> Maybe RedeemerLog
        getUpdatedRedeemerLog redeemerLog errorSource' =
            case errorSource' of
                NoError -> Just redeemerLog
                SelfRedeemer -> Just redeemerLog
                OtherRedeemer maybeIdentifier ->
                    case maybeIdentifier of
                        P.Just (RedeemerIdentifierValidator newRedeemer) ->
                            Just $ RedeemerLogValidator (P.Just newRedeemer)
                        P.Just (RedeemerIdentifierPolicy newRedeemer) ->
                            Just $ RedeemerLogPolicy (P.Just newRedeemer)
                        _ -> P.Nothing
                CombinedRedeemerResults -> P.Nothing
        -------------------
        testConfig = getTestConfig configPath configTestTree
        -------------------
        errorSource = getUpdatedRedeemerLog selectedRedeemer (tcErrorSource testConfig)
        expectedOutcome = tcExpectedOutcome testConfig
        -------------------
        generateSimpleTestCase :: GHC.HasCallStack => Maybe Tasty.TestTree
        generateSimpleTestCase =
            case expectedOutcome of
                TestSuccess ->
                    -- DebugTrace.trace ("TestSuccess case: " ++ testDescription) $
                                Just
                                    (Tasty.testCase (testDescription ++ " must succeed") $ do
                                        -- DebugTrace.traceIO $ "TestSuccess case: " ++ testDescription
                                        -- DebugTrace.traceIO "Before creating tc_"
                                        let tc_ = modifyTestParamFn defaultTestCaseParams
                                        -- DebugTrace.traceIO $ "After creating tc_: " ++ show tc_
                                        -- DebugTrace.traceIO "Before creating ctx_"
                                        let ctx_ = context_Gen tp (txSpecs []) tc_
                                        -- DebugTrace.traceIO $ "After creating ctx_: " ++ show ctx_
                                        -- DebugTrace.traceIO "Before testContextWrapper"
                                        results <- testContextWrapper tp ctx_
                                        -- DebugTrace.traceIO $ "After testContextWrapper, results: " ++ show results
                                        let newFn = assertResultsContainAnyOfExtendedMsg $
                                                        "\nSOURCE: " ++ show errorSource ++
                                                        "\nROUTE: " ++ basePath ++ configPath ++
                                                        "\nTEST CASE: " ++ show tc_ ++
                                                        -- \nCONTEXT: " ++ show ctx_ ++
                                                        "\n"
                                        -- DebugTrace.traceIO $ "NewFn created with path: " ++ (basePath ++ configPath)
                                        -- DebugTrace.traceIO "Before asserting results"
                                        newFn (errorSource, results) []
                                        -- DebugTrace.traceIO "After asserting results"
                                    )
                TestFailure errMsg ->
                    -- DebugTrace.trace ("TestFailure: " ++
                    --                     "\n") $
                    Just
                    (Tasty.testCase (testDescription ++ " must fail (" P.++ show errorSource P.++ ": " P.++ show errMsg P.++ ")") $ do
                        let tc_ = modifyTestParamFn defaultTestCaseParams
                            ctx_ = context_Gen tp (txSpecs []) tc_
                        results <- testContextWrapper tp ctx_
                        let newFn = assertResultsContainAnyOfExtendedMsg $
                                        "\nSOURCE: " ++ show errorSource ++
                                        "\nROUTE: " ++ basePath ++ configPath ++
                                        "\nTEST CASE: " ++ show tc_ ++
                                        -- "\nCONTEXT: " ++ show ctx_ ++
                                        "\n"
                        (errorSource, results) `newFn` errMsg)
                TestNone -> Nothing
        -------------------
        generateQuickCheckTestCase :: Maybe Tasty.TestTree
        generateQuickCheckTestCase =
            let
                --------------------
                matchWithOrWithoutWildcard :: DataText.Text -> DataText.Text -> Bool
                matchWithOrWithoutWildcard pattern' text =
                    if "*" `DataText.isInfixOf` pattern'
                    then
                        let regex = DataText.unpack $ DataText.replace "*" ".*" pattern'
                        in DataText.unpack text Regex.=~ regex
                    else
                        pattern' == text
                --------------------
                prop = QC.forAll (genTxParams txParamsGenerators_List configPath) $ \params ->
                    QCM.monadicIO $ do
                        let tc_ = modifyTestParamFn defaultTestCaseParams
                            ctx_ = context_Gen tp (txSpecs params) tc_
                        results <- QCM.run $ testContextWrapper tp ctx_
                        let failureInfo = "\nSOURCE: " ++ show errorSource ++
                                        "\nROUTE: " ++ basePath ++ configPath ++
                                        "\nTEST CASE: " ++ show tc_ ++
                                        "\nCONTEXT: " ++ show ctx_ ++
                                        "\nPARAMS: " ++ show params
                            actualErrors = findLogsByKey errorSource results
                        case expectedOutcome of
                            TestSuccess ->
                                if null actualErrors
                                then QCM.assert True
                                else do
                                    QCM.run $ putStrLn $ failureInfo ++ "\nUnexpected errors found: " ++ show actualErrors
                                    QCM.assert False
                            TestFailure errMsg ->
                                let
                                    matchAnyExpected :: DataText.Text -> Bool
                                    matchAnyExpected actualErr = any (`matchWithOrWithoutWildcard` actualErr) (DataText.pack <$> errMsg)

                                    missingErrors = filter (\expected -> not $ any (matchWithOrWithoutWildcard expected) actualErrors) (DataText.pack <$> errMsg)
                                    unexpectedErrors = filter (not . matchAnyExpected) actualErrors

                                    testPassed =
                                        if "" `elem` errMsg
                                            then
                                                not (null actualErrors)
                                            else
                                                case errMsg of
                                                [] -> not (null actualErrors)  -- If errMsg is empty, we expect at least one error
                                                _  -> null missingErrors       -- Otherwise, all expected errors should be present
                                in
                                    if testPassed
                                    then QCM.assert True
                                    else do
                                        QCM.run $ putStrLn $ failureInfo ++
                                            "\nExpected errors: " ++ show errMsg ++
                                            "\nActual errors: " ++ show actualErrors ++
                                            "\nMissing expected errors: " ++ show missingErrors ++
                                            "\nUnexpected errors: " ++ show unexpectedErrors
                                        QCM.assert False
                            TestNone ->
                                QCM.assert True
                expectedOutcomeStr = case expectedOutcome of
                        TestSuccess        -> " must succeed"
                        TestFailure errMsg -> " must fail (" ++ show errorSource ++ ": " ++ show errMsg ++ ")"
                        TestNone           -> " must not be executed"
            in Just $ TastyQC.testProperty (testDescription ++ expectedOutcomeStr) prop
        -------------------
    in
        -- DebugTrace.trace ("generateTestCase: " ++
        --                                 "SOURCE: " ++ show errorSource ++
        --                                 "\nROUTE: " ++ basePath ++ configPath ++
        --                                 "\n"
        --                                 ) $
        -------------------
        case expectedOutcome of
            TestNone -> Nothing
            _ -> if null txParamsGenerators_List
                 then generateSimpleTestCase
                 else generateQuickCheckTestCase
----------------------------------------------------------------------------------------
