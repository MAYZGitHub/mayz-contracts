--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module TestUtils.Automatic.TestConfigTree where

--------------------------------------------------------------------------------

-- Non-IOG imports

import qualified Data.Map                        as DataMap
import           Prelude                         as P hiding ((<>))

-- IOG imports

-- Project imports

import           TestUtils.Automatic.Helpers
import           TestUtils.Automatic.HelpersMAYZ
import           TestUtils.Automatic.Types
import           TestUtils.Types
import           TestUtils.TypesMAYZ

------------------------------------------------------------------------

-- Get the TestConfigTree for a specific test entity
getTestConfigTree :: TestParams -> TxSpecs -> RedeemerTestConfigTree
getTestConfigTree tp txSpecs =
    let

    in RedeemerTestConfigTree {
        tctInputsRef = map (createInputRefTree tp. fst) (tsInputsRef txSpecs),
        tctInputs = map (createInputTree tp . fst3) (tsInputs txSpecs),
        tctOutputs = map (createOutputTree tp . fst) (tsOutputs txSpecs),
        tctMints = map (createMintTree .  pretty . fst3) (tsMints txSpecs),
        tctValidyRange = createFullConfigTestTree [
            ("RangeValid", leafValidConfigTestTree),
            ("RangeNone", leafInvalidConfigTestTree "Missing time range"),
            ("RangeInvalid", leafInvalidConfigTestTree "Invalid time range")
        ],
        tctSignatures = createFullConfigTestTree [
            ("SignatureValid", leafValidConfigTestTree),
            ("SignatureNone", leafInvalidConfigTestTree "Missing required signature"),
            ("SignatureOther", leafInvalidConfigTestTree "Unexpected signature present")
        ]
        ,tctExtras = createFullConfigTestTree $ map (\(key, _) -> (key, leafInvalidConfigTestTree "")) (tsExtras txSpecs)
    }

----------------------------------------------------------------------------------------

createFullConfigTestTree :: [(String, ConfigTestTree)] -> ConfigTestTree
createFullConfigTestTree []    = leafInvalidConfigTestTree "Default error"
createFullConfigTestTree items = TestConfigBranch $ DataMap.fromList items

branchConfigTestTree :: [(String, ConfigTestTree)] -> ConfigTestTree
branchConfigTestTree = TestConfigBranch . DataMap.fromList

leafInvalidConfigTestTree :: String -> ConfigTestTree
leafInvalidConfigTestTree errorMsg = TestConfigLeaf TestConfig {
        tcExpectedOutcome = TestFailure [errorMsg],
        tcErrorSource = SelfRedeemer
    }

leafInvalidOtherSourceConfigTestTree :: String -> ErrorSource -> ConfigTestTree
leafInvalidOtherSourceConfigTestTree errorMsg errSource = TestConfigLeaf TestConfig {
        tcExpectedOutcome = TestFailure [errorMsg],
        tcErrorSource = errSource
    }

lealeafInvalidErrorInScriptWithNotTraceConfigTestTree :: ConfigTestTree
lealeafInvalidErrorInScriptWithNotTraceConfigTestTree  = TestConfigLeaf TestConfig {
        tcExpectedOutcome = TestFailure ["ERROR EVALUATING SCRIPT"],
        tcErrorSource = SelfRedeemer
    }

leafValidConfigTestTree :: ConfigTestTree
leafValidConfigTestTree = TestConfigLeaf TestConfig {
    tcExpectedOutcome = TestSuccess,
    tcErrorSource = NoError
}

leafNoTestConfigTestTree :: ConfigTestTree
leafNoTestConfigTestTree = TestConfigLeaf TestConfig {
    tcExpectedOutcome = TestNone,
    tcErrorSource = NoError
}

----------------------------------------------------------------------------------------

-- Helper function to create a tree for input references
createInputRefTree :: TestParams -> TestEntity -> ConfigTestTree
createInputRefTree tp entity =
    let
        otherTokensConfig = testInvalidValueOtherTokens $ getEntityTestConfig tp entity
    in
        createFullConfigTestTree [
            ("InputRefValid", leafValidConfigTestTree),
            ("InputRefInvalid", createTxOutInvalidTree entity "input ref" otherTokensConfig)
        ]

-- Helper function to create a tree for inputs
createInputTree :: TestParams -> TestEntity -> ConfigTestTree
createInputTree tp entity =
    let
        otherTokensConfig = testInvalidValueOtherTokens $ getEntityTestConfig tp entity
        desc = pretty entity
    in
        createFullConfigTestTree [
            ("InputValid", leafValidConfigTestTree),
            ("InputInvalid", branchConfigTestTree [
                ("InputInvalidRedeemer", createInvalidRedeemerTree desc),
                ("InputInvalidTxOut", createTxOutInvalidTree entity "input" otherTokensConfig)
            ])
    ]

-- Helper function to create a tree for outputs
createOutputTree :: TestParams -> TestEntity -> ConfigTestTree
createOutputTree tp entity =
    let
        otherTokensConfig = testInvalidValueOtherTokens $ getEntityTestConfig tp entity
        -- desc = pretty entity
    in
        createFullConfigTestTree [
            ("OutputValid", leafValidConfigTestTree),
            ("OutputInvalid", createTxOutInvalidTree entity "output" otherTokensConfig)
        ]

-- Helper function to create a tree for mints
createMintTree :: String -> ConfigTestTree
createMintTree desc = createFullConfigTestTree [
        ("MintValid", leafValidConfigTestTree),
        ("MintInvalid", branchConfigTestTree [
            ("MintInvalidRedeemer", createInvalidRedeemerTree desc),
            ("MintInvalidValue", createInvalidTokenTree desc "mint")
        ])
    ]

-- Helper function to create a tree for TxOutInvalid options
createTxOutInvalidTree :: TestEntity -> String -> [InvalidValueOtherToken] -> ConfigTestTree
createTxOutInvalidTree entity whereDesc otherTokensConfig =
    let
        desc = pretty entity
    in
        branchConfigTestTree [
            ("TxOutInvalidNone", leafInvalidConfigTestTree $ "Expected exactly one " ++ desc ++ " " ++ whereDesc),
            ("TxOutInvalidMore", leafInvalidConfigTestTree $ "Expected exactly one " ++ desc ++ " " ++ whereDesc),
            ("TxOutInvalidEntity", branchConfigTestTree [
                ("TxOutInvalidEntityValue", createInvalidValueTree entity whereDesc otherTokensConfig),
                ("TxOutInvalidEntityDatum", createInvalidEntityDatumTree desc),
                ("TxOutInvalidEntityAddress", leafInvalidConfigTestTree $ "Expected exactly one " ++ desc ++ " " ++ whereDesc)
            ])
        ]

-- Helper function to create a tree for InvalidValue options
createInvalidValueTree :: TestEntity -> String -> [InvalidValueOtherToken] -> ConfigTestTree
createInvalidValueTree entity whereDesc otherTokensConfig =
    let
        desc = pretty entity
        createOtherTokenTree (InvalidValueOtherToken tokenName _ _) =
            (tokenName, createInvalidTokenTree (desc ++ " with Token " ++ tokenName) whereDesc)
    in
        branchConfigTestTree [
            ("TxOutInvalidEntityValueID", createInvalidTokenTree desc whereDesc),
            ("TxOutInvalidEntityValueADA", createInvalidTokenAmountTree desc whereDesc),
            ("TxOutInvalidEntityValueOtherTokens",
                branchConfigTestTree (map createOtherTokenTree otherTokensConfig))
        ]

-- Helper function to create a tree for InvalidToken options
createInvalidTokenTree :: String ->  String -> ConfigTestTree
createInvalidTokenTree desc whereDesc  =
    case whereDesc of
        "mint" -> branchConfigTestTree [
            ("InvalidTokenCS", leafInvalidConfigTestTree $ "not isMinting" ++ desc),
            ("InvalidTokenTN", leafInvalidConfigTestTree $ "not isMinting" ++ desc),
            ("InvalidTokenAmount", createInvalidTokenAmountTree desc whereDesc)]
        "burn" -> branchConfigTestTree [
            ("InvalidTokenCS", leafInvalidConfigTestTree $ "not isBurning" ++ desc),
            ("InvalidTokenTN", leafInvalidConfigTestTree $ "not isBurning" ++ desc),
            ("InvalidTokenAmount", createInvalidTokenAmountTree desc whereDesc)]
        _ ->
            branchConfigTestTree [
            ("InvalidTokenCS", leafInvalidConfigTestTree $ "Expected exactly one " ++ desc ++ " " ++ whereDesc),
            ("InvalidTokenTN", leafInvalidConfigTestTree $ "Expected exactly one " ++ desc ++ " " ++ whereDesc),
            ("InvalidTokenAmount", createInvalidTokenAmountTree desc whereDesc)]


-- Helper function to create a tree for InvalidTokenAmount options
createInvalidTokenAmountTree :: String -> String -> ConfigTestTree
createInvalidTokenAmountTree desc whereDesc =
    case whereDesc of
        "mint" -> branchConfigTestTree [
            ("InvalidValueZero", leafInvalidConfigTestTree $ "not isMinting" ++ desc),
            ("InvalidValueLess", leafInvalidConfigTestTree $ "not isMinting" ++ desc),
            ("InvalidValueMore", leafInvalidConfigTestTree $ "not isMinting" ++ desc)]
        "burn" -> branchConfigTestTree [
            ("InvalidValueZero", leafInvalidConfigTestTree $ "not isBurning" ++ desc),
            ("InvalidValueLess", leafInvalidConfigTestTree $ "not isBurning" ++ desc),
            ("InvalidValueMore", leafInvalidConfigTestTree $ "not isBurning" ++ desc)]
        _ ->  branchConfigTestTree [
            ("InvalidValueZero", leafInvalidConfigTestTree $ "Expected exactly one " ++ desc ++ " " ++ whereDesc),
            ("InvalidValueLess", leafInvalidConfigTestTree $ "Expected exactly one " ++ desc ++ " " ++ whereDesc),
            ("InvalidValueMore", leafInvalidConfigTestTree $ "Expected exactly one " ++ desc ++ " " ++ whereDesc)]

-- Helper function to create a tree for InvalidEntityDatum options
createInvalidEntityDatumTree :: String -> ConfigTestTree
createInvalidEntityDatumTree desc = branchConfigTestTree [
        ("InvalidEntityDatumData", leafInvalidConfigTestTree $ "Invalid datum data for " ++ desc),
        ("InvalidEntityDatumNonExist", leafInvalidConfigTestTree "InvalidDatumData must be implemented and correct error message set"),
        ("InvalidEntityDatumType", lealeafInvalidErrorInScriptWithNotTraceConfigTestTree)
    ]

-- Helper function to create a tree for InvalidRedeemer options
createInvalidRedeemerTree :: String -> ConfigTestTree
createInvalidRedeemerTree desc = branchConfigTestTree [
        ("InvalidRedeemerData", leafInvalidConfigTestTree $ "Invalid redeemer data for " ++ desc),
        ("InvalidRedeemerNonExist", leafInvalidConfigTestTree "InvalidRedeemerData must be implemented and correct error message set"),
        ("InvalidRedeemerType", lealeafInvalidErrorInScriptWithNotTraceConfigTestTree)
    ]

----------------------------------------------------------------------------------------

-- Update functions that receive a tree and a path as a list of strings

-- Update path in the tree
updateConfigTree' :: ConfigTestTree -> [String] -> ConfigTestTree -> ConfigTestTree
updateConfigTree' _ [] newSubTree = newSubTree  -- Update when path is empty (leaf update)
updateConfigTree' (TestConfigBranch map') (key:restPath) newSubTree =
    case DataMap.lookup key map' of
        Just subTree ->
            let updatedSubTree = updateConfigTree' subTree restPath newSubTree  -- Continue traversing
            in TestConfigBranch $ DataMap.insert key updatedSubTree map'
        Nothing -> error $ "Invalid path for test configuration: key '" ++ key ++ "' not found"
updateConfigTree' _ (_:_) _ = error "Invalid config Tree: path is invalid or it's a leaf"  -- Error if path has more segments but is a leaf

-- Update multiple paths in the tree
updateConfigTreeMultiple' :: ConfigTestTree -> [[String]] -> ConfigTestTree -> ConfigTestTree
updateConfigTreeMultiple' tree [] _ = tree  -- No paths to update, return the original tree
updateConfigTreeMultiple' tree (path:paths) newSubTree =
    let updatedTree = updateConfigTree' tree path newSubTree
    in updateConfigTreeMultiple' updatedTree paths newSubTree

-- New function to handle multiple updates, each with its own paths and subtree
updateConfigTreeBatch' :: ConfigTestTree -> [([[String]], ConfigTestTree)] -> ConfigTestTree
updateConfigTreeBatch' = foldl applyUpdate
  where
    applyUpdate currentConfig (paths, newSubTree) =
        updateConfigTreeMultiple' currentConfig paths newSubTree

---------------------------------

-- Update path in the tree
updateConfigTree :: ConfigTestTree -> String -> ConfigTestTree -> ConfigTestTree
updateConfigTree tree pathStr = updateConfigTreeList tree (splitPath pathStr)

-- The actual update function working with a list of path components
updateConfigTreeList :: ConfigTestTree -> [String] -> ConfigTestTree -> ConfigTestTree
updateConfigTreeList _ [] newSubTree = newSubTree  -- Update when path is empty (leaf update)
updateConfigTreeList (TestConfigBranch map') (key:restPath) newSubTree =
    case DataMap.lookup key map' of
        Just subTree ->
            let updatedSubTree = updateConfigTreeList subTree restPath newSubTree  -- Continue traversing
            in TestConfigBranch $ DataMap.insert key updatedSubTree map'
        Nothing -> error $ "Invalid path for test configuration: key '" ++ key ++ "' not found"
updateConfigTreeList _ (_:_) _ = error "Invalid config Tree: path is invalid or it's a leaf"  -- Error if path has more segments but is a leaf

-- Update multiple paths in the tree
updateConfigTreeMultiple :: ConfigTestTree -> [String] -> ConfigTestTree -> ConfigTestTree
updateConfigTreeMultiple tree [] _ = tree  -- No paths to update, return the original tree
updateConfigTreeMultiple tree (path:paths) newSubTree =
    let updatedTree = updateConfigTree tree path newSubTree
    in updateConfigTreeMultiple updatedTree paths newSubTree

-- New function to handle multiple updates, each with its own paths and subtree
updateConfigTreeBatch :: ConfigTestTree -> [([String], ConfigTestTree)] -> ConfigTestTree
updateConfigTreeBatch = foldl applyUpdate
  where
    applyUpdate currentConfig (path, newSubTree) =
        updateConfigTreeMultiple currentConfig path newSubTree

----------------------------------------------------------------------------------------

-- Helper function to get a TestConfig from a ConfigTestTree
getTestConfig' :: [String] -> ConfigTestTree -> TestConfig
getTestConfig' [] (TestConfigLeaf config) = config
getTestConfig' (key:keys) (TestConfigBranch map') =
    case DataMap.lookup key map' of
        Just subTree -> getTestConfig' keys subTree
        Nothing      -> error $ "Invalid path for test configuration: key '" ++ key ++ "' not found"
getTestConfig' path _ = error $ "Invalid path '" ++ show path ++ "' for test configuration: unexpected tree structure"

---------------------------------

-- Helper function to get a TestConfig from a ConfigTestTree using a dot-separated path string
getTestConfig :: String -> ConfigTestTree -> TestConfig
getTestConfig pathStr = getTestConfig' (splitPath pathStr)

----------------------------------------------------------------------------------------
