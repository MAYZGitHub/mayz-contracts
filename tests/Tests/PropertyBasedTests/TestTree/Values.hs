--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4
{- |
Module      : Tests.UnitTests.TestTree.Values
Description : Unit tests for Plutus Ledger value operations.

This module provides unit tests for operations related to Ledger values.

It tests functionalities like flattening of values, equality checks, and
utility functions that help in the creation and manipulation of Ledger values.
-}
module TestTree.Values where

-- Non-IOG imports
import qualified Data.List                             as DataList
import           Prelude                               hiding (length)
import qualified Test.Tasty                            as Tasty
import qualified Test.Tasty.QuickCheck                 as TastyQC

-- IOG imports
import qualified Ledger.Value                          as LedgerValue
import qualified Plutus.V2.Ledger.Api                  as LedgerApiV2
import qualified PlutusTx.AssocMap                     as TxAssocMap
import           PlutusTx.Prelude                      (length)
-- Project imports
import qualified Generic.OnChainHelpers                as OnChainHelpers
import           TestUtils.QuickCheckGen.QuickCheckGen ()
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------
-- Auxiliary Functions
--------------------------------------------------------------------------------

{- | Creates a mock map of currency symbols to tokens from a given list
 representation.
-}
createMockMap ::
    [(LedgerApiV2.CurrencySymbol, [(LedgerApiV2.TokenName, Integer)])] ->
    TxAssocMap.Map
        LedgerApiV2.CurrencySymbol
        (TxAssocMap.Map LedgerApiV2.TokenName Integer)
createMockMap entries =
    TxAssocMap.fromList
        [ (cs, TxAssocMap.fromList tokens)
        | (cs, tokens) <- entries
        ]

--------------------------------------------------------------------------------
-- Test-Suite
--------------------------------------------------------------------------------

value_Tests :: AppM Tasty.TestTree
value_Tests =  do
    return $ Tasty.testGroup
        "Tests"
        [ TastyQC.testProperty
            "flattenValue preserves total token count"
            prop_flattenValue_PreservesLength
         , TastyQC.testProperty
             "flattenValue matches PlutusTx flattenValue"
             prop_compare_flattenValue
         , TastyQC.testProperty
             "isEqValue behaves same as default equality"
             prop_compare_isEqValue
         , TastyQC.testProperty
             "flattenValueToValue is the inverse of flattenValue"
             prop_compare_flattenValueToValue
        ]

--------------------------------------------------------------------------------

-- -- Wrapper to convert the property into a QuickCheck-compatible form
-- prop_flattenValue_PreservesLength_QC :: LedgerValue.Value -> PropertyM AppM ()
-- prop_flattenValue_PreservesLength_QC value = do
--     result <- run $ prop_flattenValue_PreservesLength value
--     assert result

{- | Test if 'flattenValue' maintains the same total number of token entries
 after flattening.
-}
prop_flattenValue_PreservesLength :: LedgerValue.Value ->  Bool
prop_flattenValue_PreservesLength value =
    let
        flattenValuesNew :: LedgerValue.Value -> [(LedgerValue.AssetClass, Integer)]
        flattenValuesNew value' =
            [(assetClass, amount) | (cs, tokens) <- TxAssocMap.toList (LedgerValue.getValue value')
                                , (tn, amount) <- TxAssocMap.toList tokens
                                , let assetClass = LedgerValue.assetClass cs tn]

        countPositiveEntries :: LedgerValue.Value -> Integer
        countPositiveEntries value' =
            length . filter (\(_, amt) -> amt /= 0) $ flattenValuesNew value'

        flattened = OnChainHelpers.flattenValue value
        tokenEntriesCount =countPositiveEntries  value

    in
    --    DebugTrace.trace ("Testing length flattened: " ++ (show $ length flattened)) $
    --         DebugTrace.trace ("Testing tokenEntriesCount: " ++ show tokenEntriesCount) $
                    length flattened == tokenEntriesCount

{- | Test if the implementation of 'flattenValue' produces the same result as
 LedgerValue's built-in function.
-}
prop_compare_flattenValue :: LedgerValue.Value -> Bool
prop_compare_flattenValue value =
    -- DebugTrace.trace ("Testing value: " ++ show value) $
    let flatten1 = LedgerValue.flattenValue value
        flatten2 = OnChainHelpers.flattenValue value
        -- Sorting both lists
        sortedFlatten1 = DataList.sortBy (\(cs1, tn1, amt1) (cs2, tn2, amt2) ->
                                     compare cs1 cs2 <> compare tn1 tn2 <> compare amt1 amt2) flatten1
        sortedFlatten2 = DataList.sortBy (\(cs1, tn1, amt1) (cs2, tn2, amt2) ->
                                     compare cs1 cs2 <> compare tn1 tn2 <> compare amt1 amt2) flatten2
    in sortedFlatten1 == sortedFlatten2

{- | Test if our 'isEqValue' function behaves identically to the default
 equality operator.
-}
prop_compare_isEqValue :: LedgerValue.Value -> LedgerValue.Value -> Bool
prop_compare_isEqValue v1 v2 = OnChainHelpers.isEqValue v1 v2 == (v1 == v2)

{- | Test if the herlper 'flattenValueToValue' function behaves as the
     inverse of flattenValue.
-}
prop_compare_flattenValueToValue :: LedgerValue.Value -> Bool
prop_compare_flattenValueToValue value =
    OnChainHelpers.flattenValueToValue (LedgerValue.flattenValue value) == value

--------------------------------------------------------------------------------
