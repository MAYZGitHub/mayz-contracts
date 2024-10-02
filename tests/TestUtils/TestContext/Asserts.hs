{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2
module TestUtils.TestContext.Asserts where
--------------------------------------------------------------------------------2

-- Non-IOG imports
import qualified Data.Text            as DataText
import qualified GHC.Stack            as GHC
import           Prelude              as P
import qualified Test.Tasty.HUnit     as Tasty
import qualified Text.Regex.TDFA      as Regex

-- IOG imports
import qualified Plutus.V2.Ledger.Api as LedgerApiV2

-- Project imports
import           TestUtils.TypesMAYZ


------------------------------------------------------------------------------

-- Helper function to find logs by key and combine them if there are multiple matches,
-- and if the key is not found, default to the logs associated with `Nothing`.
findLogsByKey :: Maybe RedeemerLog
            -> [(Maybe RedeemerLog, (LedgerApiV2.LogOutput, b, c))]
            -> LedgerApiV2.LogOutput
findLogsByKey key results' =
    let matchingEntries = filter (matchKey key) results'
    in if not (null matchingEntries)
    then foldMap (\(_, (logs', _, _)) -> logs') matchingEntries
    else foldMap (\(_, (logs', _, _)) -> logs') (filter (matchKey Nothing) results')
    where
        matchKey :: Maybe RedeemerLog
                -> (Maybe RedeemerLog, (LedgerApiV2.LogOutput, b, c))
                -> Bool
        matchKey Nothing (Nothing, _)                                                               = True  -- If both are Nothing, they match
        matchKey (Just (RedeemerLogValidator redeemer1)) (Just (RedeemerLogValidator redeemer2), _) = redeemer1 == redeemer2  -- Compare Minting Redeemers
        matchKey (Just (RedeemerLogPolicy redeemer1)) (Just (RedeemerLogPolicy redeemer2), _)       = redeemer1 == redeemer2  -- Compare Spending Redeemers
        -- matchKey (Just (_ redeemer1)) (Just (_ redeemer2), _)       = redeemer1 == redeemer2  -- Compare Other Redeemers
        matchKey _ _                                                                                = False  -- Different keys don't match

--------------------------------------------------------------------------------

assertResultsContainAnyOfExtendedMsg ::  GHC.HasCallStack => String -> (Maybe RedeemerLog, [(Maybe RedeemerLog, (LedgerApiV2.LogOutput, b, c))]) -> [String] -> Tasty.Assertion
assertResultsContainAnyOfExtendedMsg msg (selectedRedeemer, results) searchAny =
    let
        useMsg = if msg == "" then "" else "\n< " ++ msg ++ " >"
        --------------------
        -- Assert that logs contain any of the specified strings
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
        assertLogsContainAnyOf :: LedgerApiV2.LogOutput -> LedgerApiV2.LogOutput -> Tasty.Assertion
        assertLogsContainAnyOf logs' searchAny' = do
            if "" `elem` searchAny'
            then
                -- If the empty string is in searchAny, we expect any log message to match
                assertResultsContainError (selectedRedeemer, results)
            else
                case searchAny' of
                    [] ->
                        Tasty.assertBool
                            ("Unexpected logs found. Expected none. Found: " ++ show logs' ++ useMsg)
                            (null logs')
                    _  ->
                        Tasty.assertBool
                            ("None of the expected logs found. Expected one of: " ++ show searchAny' ++ ". Found: " ++ show logs' ++ useMsg)
                            (any (\pattern' -> any (matchWithOrWithoutWildcard pattern') logs') searchAny')
        --------------------
        logs = findLogsByKey selectedRedeemer results
        --------------------
    in assertLogsContainAnyOf logs (map DataText.pack searchAny)

-- Assert that the logs for a specific redeemer contain any of the specified strings
assertResultsContainAnyOf ::  GHC.HasCallStack => (Maybe RedeemerLog, [(Maybe RedeemerLog, (LedgerApiV2.LogOutput, b, c))]) -> [String] -> Tasty.Assertion
assertResultsContainAnyOf  = assertResultsContainAnyOfExtendedMsg ""

--------------------------------------------------------------------------------

assertResultsContainAnyOfIfCondition ::  GHC.HasCallStack => (Maybe RedeemerLog, [(Maybe RedeemerLog, (LedgerApiV2.LogOutput, b, c))]) -> [String] -> Bool -> Tasty.Assertion
assertResultsContainAnyOfIfCondition (selectedRedeemer, results) searchAny condition =
    if condition then
        assertResultsContainAnyOfExtendedMsg "" (selectedRedeemer, results) searchAny
    else
        assertResultsContainNoError (selectedRedeemer, results)

--------------------------------------------------------------------------------

-- Assert that the logs for a specific redeemer contain any error (non-empty logs)
assertResultsContainError :: GHC.HasCallStack => (Maybe RedeemerLog, [(Maybe RedeemerLog, (LedgerApiV2.LogOutput, b, c))]) -> Tasty.Assertion
assertResultsContainError (selectedRedeemer, results) =
    let
        -- Assert that logs contain any entry (indicating some error)
        assertLogsContainError :: LedgerApiV2.LogOutput -> Tasty.Assertion
        assertLogsContainError logs' =
            Tasty.assertBool
                ("Expected an error, but found none. Logs: " ++ show logs')
                (not (null logs'))

        -- Find the logs for the selected redeemer
        logs = findLogsByKey selectedRedeemer results
    in assertLogsContainError logs

assertResultsContainNoError :: GHC.HasCallStack => (Maybe RedeemerLog, [(Maybe RedeemerLog, (LedgerApiV2.LogOutput, b, c))]) -> Tasty.Assertion
assertResultsContainNoError (selectedRedeemer, results) =
    assertResultsContainAnyOf (selectedRedeemer, results) []

--------------------------------------------------------------------------------
