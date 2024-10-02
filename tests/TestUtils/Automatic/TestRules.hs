
{-# LANGUAGE CPP #-}
--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module TestUtils.Automatic.TestRules where

---------------------------------------------------------------------------------

-- Non-IOG imports

import qualified Codec.Xlsx                  as XLSX
import qualified Control.Exception           as Exception
import qualified Control.Lens                as Lens
import qualified Data.ByteString.Lazy        as BL
import qualified Data.List                   as DataList
import qualified Data.List.Split             as DataList (splitOn)
import qualified Data.Map                    as DataMap
import qualified Data.Maybe                  as DataMaybe
import qualified Data.Text                   as DataText
import           Prelude                     as P hiding ((<>))
import qualified System.Directory            as SystemDirectory
import qualified Text.Regex.TDFA             as Regex

-- IOG imports

-- Project imports

import           TestUtils.Automatic.Helpers
import           TestUtils.Automatic.Types
import           TestUtils.Types
import           TestUtils.TypesMAYZ
import TestUtils.Helpers

------------------------------------------------------------------------

-- Read Rules from Excel
readRuleTreeFromExcel :: Bool -> FilePath -> IO RuleTree
readRuleTreeFromExcel swTrace excelFilePath = do
    --------------------
    putStrLn $ "Reading Excel: " ++ excelFilePath
    --------------------
    excelResult <- readExcelRows swTrace excelFilePath
    --------------------
    case excelResult of
        Left err -> P.error ("Error reading Excel: " ++ err)
        Right rows -> do
            let rulesRows = parseRowsToRulesRows swTrace rows
                ruleTree = rulesRowsToRuleTree swTrace rulesRows
            !_ <- debugTraceIf swTrace ("\nRules Tree:\n" ++ show ruleTree)
            return ruleTree

readExcelRows :: Bool -> FilePath -> IO (Either String [CSVRow])
readExcelRows swTrace filePath = do
    let
        -----------------
        -- Extract rows from a sheet
        extractRowsFromSheet :: XLSX.Worksheet -> [[XLSX.Cell]]
        extractRowsFromSheet sheetData =
            let cells = DataMap.toList (sheetData Lens.^. XLSX.wsCells)
                rows = groupRowsByRowNumber cells
            in rows
        -----------------
        groupRowsByRowNumber :: [((XLSX.RowIndex, XLSX.ColumnIndex), XLSX.Cell)] -> [[XLSX.Cell]]
        groupRowsByRowNumber cells =
            let grouped = DataMap.fromListWith DataMap.union [(row, DataMap.singleton col cell) | ((row, col), cell) <- cells]
                colIndices = [col | (_, col) <- map fst cells]
                minCol = minimum colIndices
                maxCol = maximum colIndices
                fillRow rowMap = [DataMap.findWithDefault XLSX.Cell {
                    XLSX._cellValue = Nothing,
                    XLSX._cellStyle = Nothing,
                    XLSX._cellComment = Nothing,
                    XLSX._cellFormula = Nothing
                } col rowMap | col <- [minCol..maxCol]]
                sortedGroups = DataMap.map fillRow grouped
            in DataMap.elems sortedGroups
        -----------------
        extractRowData :: [XLSX.Cell] -> Maybe CSVRow
        extractRowData row =
            let
                cellMap = DataMap.fromList [(col, getCellValue cell) | (col, cell) <- zip [1..] row]
                getValue idx = DataMaybe.fromMaybe "" (DataMap.lookup idx cellMap)
            in
                debugTraceIf_ swTrace ("extractRowData: " ++ show cellMap) $
                    Just $ CSVRow
                        (getValue (1::Integer))  -- Use
                        (getValue 2)  -- Tx
                        (getValue 3)  -- Script
                        (getValue 4)  -- Redeemer
                        (getValue 5)  -- Group
                        (getValue 6)  -- Path
                        (getValue 7)  -- ExpectedOutcome
                        (getValue 8)  -- ErrorMessage
                        (getValue 9)  -- ErrorSource
        -----------------
        getCellValue :: XLSX.Cell -> String
        getCellValue cell =
            case cell Lens.^. XLSX.cellValue of
                Just (XLSX.CellText txt) -> DataText.unpack txt
                Just (XLSX.CellDouble d) -> show d
                Just (XLSX.CellBool b)   -> show b
                Just (XLSX.CellRich rs)  -> DataText.unpack $ DataText.concat [r Lens.^. XLSX.richTextRunText | r <- rs]
                _                        -> ""
    -----------------
    fileExists <- SystemDirectory.doesFileExist filePath
    -----------------
    if not fileExists
        then return $ Left ("File does not exist: " ++ filePath)
        else do
            result <- Exception.try $ BL.readFile filePath :: IO (Either Exception.SomeException BL.ByteString)
            case result of
                Left ex -> return $ Left ("Error reading file: " ++ show ex)
                Right bs -> do
                    let xlsx = XLSX.toXlsx bs
                    let sheets = xlsx Lens.^. XLSX.xlSheets
                    if null sheets
                        then return $ Left "No sheets found in Excel file."
                        else do
                            -- Combine data from all sheets
                            let allRows = concatMap (extractRowsFromSheet . snd) sheets
                            return $ Right (DataMaybe.mapMaybe extractRowData allRows)

-- Updated mapping function
parseRowsToRulesRows :: Bool -> [CSVRow] -> [(String, CSVRule)]
parseRowsToRulesRows swTrace rows =
    let
        -----------------
        -- Function to create a rule path string from the CSVRow
        createRulePath :: CSVRow -> String
        createRulePath row =
            let components = [csvTx row, csvScript row, csvRedeemer row, csvGroup row, csvPath row]
                -- Drop empty trailing parts and only include up to the last non-empty part
                nonEmptyComponents = reverse $ dropWhile (== "") $ reverse components
            in DataList.intercalate "." nonEmptyComponents
        -----------------
        -- Updated isValidCSVRow function with enhanced validation logic
        isValidCSVRow :: CSVRow -> Bool
        isValidCSVRow row =
            let
                swUse = csvUse row == "True"
                tx = csvTx row
                script = csvScript row
                redeemer = csvRedeemer row
                group = csvGroup row
                path = csvPath row
            in swUse && not (tx == "TX NAME" || script == "SCRIPT") &&  -- Filter out header rows
            not (null tx) &&  -- tx must always be set
            (not (null script) || (null script && null redeemer && null group && null path)) &&  -- Script is optional if all subsequent fields are empty
            (not (null redeemer) || (null redeemer && null group && null path)) &&  -- Redeemer is optional if subsequent fields are empty
            (not (null group) || (null group && null path)) &&  -- Group is optional if subsequent field (path) is empty
            (not (null path) || null path)  -- Path can be empty without conditions
        -----------------
        -- Updated helper function to calculate match scores from the rule path
        calculateMatchScores :: String -> MatchScore
        calculateMatchScores rulePath =
            let segments = DataList.splitOn "." rulePath
                levelCount = P.length segments
                firstFourSegments = take 4 segments
                remainingSegments = drop 4 segments
                countMatches segs = (P.length $ filter (\seg -> not (isWildcard seg) && seg /= "ANY") segs,
                                     P.length $ filter isWildcard segs,
                                     P.length $ filter (== "ANY") segs)
                (directFirst4, partialFirst4, anyFirst4) = countMatches firstFourSegments
                (directRest, partialRest, anyRest) = countMatches remainingSegments
            in MatchScore
                { msLevelsMatched = levelCount
                , msDirectFirst4 = directFirst4
                , msPartialFirst4 = partialFirst4
                , msAnyFirst4 = anyFirst4
                , msDirectRest = directRest
                , msPartialRest = partialRest
                , msAnyRest = anyRest
                }
        -----------------
        -- Read ExpectedOutcome and ErrorSource based on the CSV values
        readExpectedOutcome :: String -> String -> TestOutcome
        readExpectedOutcome "TestSuccess" _ = TestSuccess
        readExpectedOutcome "TestFailure" errMsg = TestFailure [errMsg]
        readExpectedOutcome "TestNone" _ = TestNone
        readExpectedOutcome outcome message =
            debugTraceIf_ swTrace ("Invalid TestOutcome: " ++ outcome ++ " - message " ++ message) $
            error "Invalid TestOutcome"
        -----------------
        readErrorSource :: CSVRow -> TestOutcome -> String -> ErrorSource
        readErrorSource _ TestSuccess _ = NoError
        readErrorSource _ TestNone _ = NoError
        readErrorSource _ (TestFailure _) "SelfRedeemer" = SelfRedeemer
        readErrorSource _ (TestFailure _) "CombinedRedeemerResults" = CombinedRedeemerResults
        readErrorSource row (TestFailure _) strErrorSource =
            case DataList.stripPrefix "OtherRedeemer " strErrorSource of
                Just actualName -> case getValidatorTestRedeemer (Just actualName) of
                    Just validatorRedeemer -> OtherRedeemer (Just (RedeemerIdentifierValidator validatorRedeemer))
                    Nothing -> case getPolicyTestRedeemer (Just actualName) of
                        Just policyRedeemer -> OtherRedeemer (Just (RedeemerIdentifierPolicy policyRedeemer))
                        Nothing             -> error $ "Redeemer not found: " ++ actualName
                Nothing -> error $ "Invalid ErrorSource: " ++ strErrorSource ++ " - row: " ++ show row
    in
        debugTraceIf_ swTrace ("parseCSVToRules: " ++ show rows )
        map (\row ->
            let
                rulePath = createRulePath row
                score = calculateMatchScores rulePath
                expectedOutCome = readExpectedOutcome (csvExpectedOutcome row) (csvErrorMessage row)
            in (rulePath, CSVRule
                { ruleExpectedOutcome = readExpectedOutcome (csvExpectedOutcome row) (csvErrorMessage row)
                , ruleErrorSource = readErrorSource row expectedOutCome (csvErrorSource row)
                , ruleScore = score
                })) (filter isValidCSVRow rows)

rulesRowsToRuleTree :: Bool -> [(String, CSVRule)] -> RuleTree
rulesRowsToRuleTree swTrace rulesRows =
    let
        -------------
        -- Helper to show the structure of the rule tree
        showRuleTreeStructure :: RuleTree -> String
        showRuleTreeStructure = go "" 0
            where
                go currentPath depth (RuleNode children maybeRule) =
                    let nodeInfo = case maybeRule of
                                    Just rule ->
                                        indent depth ++ "- Leaf:\n" ++
                                        indent depth ++ "  Path: " ++ currentPath ++ "\n" ++
                                        indent depth ++ "  Rule: " ++ show rule ++ "\n"
                                    Nothing ->
                                        if null currentPath
                                        then ""  -- Root node
                                        else indent depth ++ "- Branch: " ++ currentPath ++ "\n"
                        childrenInfo = concatMap (\(key, child) ->
                                                    let newPath = if null currentPath
                                                                    then key
                                                                    else currentPath ++ "." ++ key
                                                    in go newPath (depth + 1) child
                                                ) (DataMap.toList children)
                    in nodeInfo ++ childrenInfo
        -------------
        buildRuleTree :: [(String, CSVRule)] -> RuleTree
        buildRuleTree rules' =
            let
                insertRule :: (String, CSVRule) -> RuleTree -> RuleTree
                insertRule (path, rule) tree =
                    let pathParts = DataList.splitOn "." path
                    in insertRulePath pathParts rule tree

                insertRulePath :: [String] -> CSVRule -> RuleTree -> RuleTree
                insertRulePath [] rule (RuleNode children _) = RuleNode children (Just rule)
                insertRulePath (x:xs) rule (RuleNode children maybeRule) =
                    RuleNode (DataMap.alter (Just . insertRulePath xs rule . DataMaybe.fromMaybe (RuleNode DataMap.empty Nothing)) x children) maybeRule
            in
                foldr insertRule (RuleNode DataMap.empty Nothing) rules'
        -------------
        ruleTree = buildRuleTree rulesRows
        -------------
    in
    debugTraceIf_ swTrace ("\nRules:\n" ++ show rulesRows) $
        debugTraceIf_ swTrace ("\nRule Tree Structure:\n" ++ showRuleTreeStructure ruleTree) ruleTree

------------------------------------------------------------------------

updateConfigTreeFromRuleTree :: Bool -> String -> RedeemerLog -> TxSpecs -> RuleTree -> RedeemerTestConfigTree -> RedeemerTestConfigTree
updateConfigTreeFromRuleTree swTrace txName redeemerLog txSpecs ruleTree config =
    let basePath = txName ++ "." ++ getRedeemerScriptNameFromLog redeemerLog ++ "." ++ getRedeemerNameFromLog redeemerLog ++ "."
    in RedeemerTestConfigTree
        { tctInputsRef = updateConfigTreeListFromRuleTree swTrace ruleTree (tctInputsRef config) (basePath ++ "InputsRef") (pretty . fst <$> tsInputsRef txSpecs)
        , tctInputs = updateConfigTreeListFromRuleTree swTrace ruleTree (tctInputs config) (basePath ++ "Inputs") (pretty . fst3 <$> tsInputs txSpecs)
        , tctOutputs = updateConfigTreeListFromRuleTree swTrace ruleTree (tctOutputs config) (basePath ++ "Outputs") (pretty . fst <$> tsOutputs txSpecs)
        , tctMints = updateConfigTreeListFromRuleTree swTrace ruleTree (tctMints config) (basePath ++ "Mints") (pretty . fst3 <$> tsMints txSpecs)
        , tctValidyRange = updateConfigTreeSingleFromRuleTree swTrace ruleTree (tctValidyRange config) (basePath ++ "ValidityRange") ""
        , tctSignatures = updateConfigTreeSingleFromRuleTree swTrace ruleTree (tctSignatures config) (basePath ++ "Signatures") ""
        , tctExtras = updateConfigTreeSingleFromRuleTree swTrace ruleTree (tctExtras config) (basePath ++ "Extras") ""
        }

updateConfigTreeListFromRuleTree :: Bool -> RuleTree -> [ConfigTestTree] -> String -> [String] -> [ConfigTestTree]
updateConfigTreeListFromRuleTree swTrace ruleTree configList basePath subPathsIdentities =
    zipWith (\index config -> updateConfigTreeSingleFromRuleTree swTrace ruleTree config (basePath ++ "." ++ (subPathsIdentities !! index)) (subPathsIdentities !! index)) [0..] configList

updateConfigTreeSingleFromRuleTree :: Bool -> RuleTree -> ConfigTestTree -> String -> String -> ConfigTestTree
updateConfigTreeSingleFromRuleTree swTrace ruleTree (TestConfigLeaf config) path subPathIdentity =
    let
        ----------------
        ruleReplaceIdentityTags :: String -> TestOutcome -> TestOutcome
        ruleReplaceIdentityTags identityName inTextOutcome = case inTextOutcome of
            TestFailure errors ->
                    TestFailure (map (replace "<IDENTITY>" identityName) errors)
                where
                    replace :: String -> String -> String -> String
                    replace old new = DataList.intercalate new . DataList.splitOn old
            _ -> inTextOutcome
        ----------------
        configPath = DataList.splitOn "." path
        (matchingRule, matchedRulePath, matchScore) = findBestMatchRuleForConfigPath swTrace configPath ruleTree ""
        ----------------
    in
        debugTraceIf_ swTrace
            ("\nPath: " ++ path ++
            "\nBest match: " ++ (case matchingRule of
                                    Just _  -> matchedRulePath
                                    Nothing -> "None") ++
            "\nScore: " ++ show (msDirectFirst4 matchScore) ++ " directs in first 4, " ++
                            show (msPartialFirst4 matchScore) ++ " partial in first 4, " ++
                            show (msAnyFirst4 matchScore) ++ " any in first 4, " ++
                            show (msDirectRest matchScore) ++ " directs in rest, " ++
                            show (msPartialRest matchScore) ++ " partial in rest, " ++
                            show (msAnyRest matchScore) ++ " any in rest, " ++
                            "Total Length " ++ show (msLevelsMatched matchScore) ++
            "\nRule: " ++ (case matchingRule of
                                Just rule -> "Matched. Outcome: " ++ show (ruleExpectedOutcome rule) ++
                                            " (ErrorSource: " ++ show (ruleErrorSource rule) ++ ")"
                                Nothing -> "No match. Keeping original config.") ++ "\n"
            )
            (case matchingRule of
                Nothing -> TestConfigLeaf config
                Just rule -> TestConfigLeaf TestConfig
                    { tcExpectedOutcome = ruleReplaceIdentityTags subPathIdentity (ruleExpectedOutcome rule)
                    , tcErrorSource = ruleErrorSource rule
                    })
updateConfigTreeSingleFromRuleTree swTrace ruleTree (TestConfigBranch subTree) path subPathIdentity =
    TestConfigBranch $ DataMap.mapWithKey (\key subTree' -> updateConfigTreeSingleFromRuleTree swTrace ruleTree subTree' (path ++ "." ++ key) subPathIdentity) subTree

------------------------------------------------------------------------

findBestMatchRuleForConfigPath :: Bool -> [String] -> RuleTree -> String -> (Maybe CSVRule, String, MatchScore)
findBestMatchRuleForConfigPath swTrace configPath ruleTree initialRulePath =
    let
        ----------
        selectBestMatch :: [(Maybe CSVRule, String, MatchScore)] -> (Maybe CSVRule, String, MatchScore)
        selectBestMatch = foldl (\acc@(_, _, accScore) cur@(_, _, curScore) ->
            if compareMatchScores curScore accScore == GT then cur else acc
            ) (Nothing, "", MatchScore 0 0 0 0 0 0 0)
        ----------
        compareMatchScores :: MatchScore -> MatchScore -> Ordering
        compareMatchScores a b
            | msDirectFirst4 a /= msDirectFirst4 b = compare (msDirectFirst4 a) (msDirectFirst4 b)
            | totalFirst4 a /= totalFirst4 b = compare (totalFirst4 a) (totalFirst4 b)
            | msLevelsMatched a /= msLevelsMatched b = compare (msLevelsMatched a) (msLevelsMatched b)
            | totalDirect a /= totalDirect b = compare (totalDirect a) (totalDirect b)
            | totalPartial a /= totalPartial b = compare (totalPartial a) (totalPartial b)
            | otherwise = compare (totalAny a) (totalAny b)
            where
                totalFirst4 s = msDirectFirst4 s + msPartialFirst4 s + msAnyFirst4 s
                totalDirect s = msDirectFirst4 s + msDirectRest s
                totalPartial s = msPartialFirst4 s + msPartialRest s
                totalAny s = msAnyFirst4 s + msAnyRest s
        ----------
        findValidMatches :: String -> DataMap.Map String RuleTree -> [(MatchType, String, RuleTree)]
        findValidMatches segment children =
            let
                ----------
                isWildcardMatch :: String -> String -> Bool
                isWildcardMatch pattern' segment' =
                    let regexPattern = "^" ++ DataText.unpack (DataText.replace "*" ".*" (DataText.pack pattern')) ++ "$"
                    in segment' Regex.=~ regexPattern
                ----------
                directMatch = case DataMap.lookup segment children of
                    Just subTree -> [(Direct, segment, subTree)]
                    Nothing      -> []
                ----------
                anyMatch = case DataMap.lookup "ANY" children of
                    Just subTree -> [(Any, "ANY", subTree)]
                    Nothing      -> []
                ----------
                partialMatches = [(Partial, k, v) | (k, v) <- DataMap.toList children, isWildcard k && isWildcardMatch k segment]
                ----------
            in directMatch ++ anyMatch ++ partialMatches
        ----------
        findBestMatchHelper :: [String] -> RuleTree -> String -> Int -> (Maybe CSVRule, String, MatchScore)
        ----------
        findBestMatchHelper [] (RuleNode _ maybeRule) rulePath depth' =
            case maybeRule of
                Just rule ->
                    let score = ruleScore rule -- Use the pre-calculated score directly from the rule
                    in
                        debugTraceIf_ swTrace (indent depth' ++ "Matched rule at path: " ++ rulePath ++ "\n" ++
                                        indent depth' ++ "Score: " ++ show score) 
                            (Just rule, rulePath, score)
                Nothing -> (Nothing, rulePath, MatchScore 0 0 0 0 0 0 0)
        ----------
        findBestMatchHelper (x:xs) (RuleNode children' maybeRule) rulePath depth' =
            let matches = findValidMatches x children'
                ----------
                ruleMatch = case maybeRule of
                    Just rule ->
                            let score' = ruleScore rule
                            in
                                debugTraceIf_ swTrace (indent depth' ++ "Path continues but Matched rule at path: " ++ rulePath ++ "\n" ++
                                        indent depth' ++ "Score: " ++ show score')
                                [(Just rule, rulePath, ruleScore rule)]
                    Nothing -> []
                ----------
                results = map (\(matchType, key, subTree) ->
                    debugTraceIf_ swTrace (indent depth' ++ "Checking rule " ++ (if P.null rulePath then key else rulePath ++ "." ++ key) ++
                                    " (matches " ++ x ++ ", type: " ++ show matchType ++ ")") $
                        findBestMatchHelper xs subTree (if P.null rulePath then key else rulePath ++ "." ++ key) (depth' + 1)
                    ) matches
                ----------
                combinedResults = ruleMatch ++ results
            in selectBestMatch combinedResults
    in
        debugTraceIf_ swTrace ("Working path: " ++ DataList.intercalate "." configPath) $
        findBestMatchHelper configPath ruleTree initialRulePath 0

------------------------------------------------------------------------
