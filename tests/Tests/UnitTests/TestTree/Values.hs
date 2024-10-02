--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3
{- |
Module      : Tests.UnitTests.TestTree.Values
Description : Unit tests for Plutus Ledger value operations.

This module provides unit tests for operations related to Ledger values.

It tests functionalities like flattening of values, equality checks, and
utility functions that help in the creation and manipulation of Ledger values.
-}
module TestTree.Values where
--------------------------------------------------------------------------------3

-- Non-IOG imports
import           Prelude
import qualified Test.Tasty             as Tasty
import qualified Test.Tasty.HUnit       as Tasty

-- IOG imports

import qualified Ledger.Value           as LedgerValue
import qualified Plutus.V2.Ledger.Api   as LedgerApiV2
import qualified PlutusTx.AssocMap      as TxAssocMap
import           PlutusTx.Prelude       ()

-- Project imports
import           Generic.OnChainHelpers (flattenValue)
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------
-- Test-Suite
--------------------------------------------------------------------------------

value_Tests :: TestParams -> Tasty.TestTree
value_Tests _ =
    Tasty.testGroup
        "Tests"
        [Tasty.testCase "FlattenValue to list of tuples" test_flattenValue_Correct]

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

{- | Test if the 'flattenValue' function correctly transforms the map
 representation into a flattened list representation.
-}
test_flattenValue_Correct :: Tasty.Assertion
test_flattenValue_Correct = do
    let mockMap =
            createMockMap
                [
                    ( LedgerApiV2.CurrencySymbol "currency1"
                    ,
                        [ (LedgerApiV2.TokenName "token1", 1)
                        , (LedgerApiV2.TokenName "token2", 2)
                        ]
                    )
                ,
                    ( LedgerApiV2.CurrencySymbol "currency2"
                    ,
                        [ (LedgerApiV2.TokenName "tokenA", 10)
                        , (LedgerApiV2.TokenName "tokenB", 20)
                        ]
                    )
                ]

    let value = LedgerApiV2.Value mockMap
    let expectedFlattenedValue =
            [
                ( LedgerApiV2.CurrencySymbol "currency1"
                , LedgerApiV2.TokenName "token1"
                , 1
                )
            ,
                ( LedgerApiV2.CurrencySymbol "currency1"
                , LedgerApiV2.TokenName "token2"
                , 2
                )
            ,
                ( LedgerApiV2.CurrencySymbol "currency2"
                , LedgerApiV2.TokenName "tokenA"
                , 10
                )
            ,
                ( LedgerApiV2.CurrencySymbol "currency2"
                , LedgerApiV2.TokenName "tokenB"
                , 20
                )
            ]

    flattenValue value Tasty.@?= expectedFlattenedValue

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

-- | Mock value for zero ADAs.
zeroValue :: LedgerValue.Value
zeroValue = do
    let mockMap =
            createMockMap
                [
                    ( LedgerApiV2.CurrencySymbol "ff"
                    ,
                        [ (LedgerApiV2.TokenName "1a", 0)
                        , (LedgerApiV2.TokenName "b4", 0)
                        ]
                    )
                ,
                    ( LedgerApiV2.CurrencySymbol "cdef"
                    ,
                        [ (LedgerApiV2.TokenName "7890", 0)
                        , (LedgerApiV2.TokenName "ff00", 0)
                        ]
                    )
                ]
    LedgerApiV2.Value mockMap
