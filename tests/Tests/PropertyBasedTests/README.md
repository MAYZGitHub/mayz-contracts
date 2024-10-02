# Property-Based Tests

## Tasty Test Organization

Tests in Tasty are organized in a `TestTree`, a tree-like structure. This allows us to group related tests together and also create nested groups.

```haskell
-- | The main entry point to execute all the tests.
main :: IO ()
main = defaultMain $ Tasty.testGroup "Testing Helper Functions:"
    [ Tasty.testGroup "Unit Tests"
        [ Tasty.testCase "isEqValue should return True for identical values"
                   test_isEqValue_True
        , Tasty.testCase "flattenValue should correctly flatten Value to list of tuples"
                   test_flattenValue_Correct
        ]
    , Tasty.testGroup "Property-Based Tests"
        [ TastyQC.testProperty "flattenValue preserves total token count"
                       prop_flattenValue_PreservesLength
        , TastyQC.testProperty "isEqValue identifies identical values as equal"
                       prop_isEqValue_Identical
        ]
    ]
```

## Understanding `testProperty`

Consider the following function call:

```haskell
testProperty "flattenValue preserves total token count"
             prop_flattenValue_PreservesLength
```

The `testProperty` function is defined in the `Test.Tasty.QuickCheck` module. It takes two parameters: the name of the test, and a value with a type that has an instance of the `Testable` class. In this context, the value is `prop_flattenValue_PreservesLength`, the property we want to test.

## Testing the `flattenValue` Function

```haskell
-- | Test if 'flattenValue' maintains the same total number of token entries after flattening.
prop_flattenValue_PreservesLength :: LedgerValue.Value -> Bool
prop_flattenValue_PreservesLength value =
    let flattened = flattenValue value
        tokenEntriesCount = sum . map (length . TxAssocMap.toList . snd) $
            TxAssocMap.toList (LedgerValue.getValue value)
    in length flattened == tokenEntriesCount
```

### How a `Value` is represented

```haskell
TxAssocMap.fromList
            [ (LedgerApiV2.CurrencySymbol "currency1"
              , TxAssocMap.fromList
                  [ (LedgerApiV2.TokenName "token1", 1)
                  , (LedgerApiV2.TokenName "token2", 2)
                  ]
              )
            , (LedgerApiV2.CurrencySymbol "currency2"
              , TxAssocMap.fromList
                  [ (LedgerApiV2.TokenName "tokenA", 10)
                  , (LedgerApiV2.TokenName "tokenB", 20)
                  ]
              )
            ]
```

After being flattened, it becomes:

```haskell
[ (LedgerApiV2.CurrencySymbol "currency1", LedgerApiV2.TokenName "token1", 1)
, (LedgerApiV2.CurrencySymbol "currency1", LedgerApiV2.TokenName "token2", 2)
, (LedgerApiV2.CurrencySymbol "currency2", LedgerApiV2.TokenName "tokenA", 10)
, (LedgerApiV2.CurrencySymbol "currency2", LedgerApiV2.TokenName "tokenB", 20)
]
```

Here's what `prop_flattenValue_PreservesLength` does:

- `flattened` extracts a flat list of all token entries from the value.
- `tokenEntriesCount` computes the total number of token entries. It first converts the value to an associative map, iterates over each entry to count its internal token entries, and finally sums these counts.

If the length of `flattened` is equal to `tokenEntriesCount`, it indicates that both representations yield consistent results across a wide range of entries, suggesting that they are semantically equivalent.

## Generating Random Entries with QuickCheck

QuickCheck handles the generation of a broad range of random entries. The key step is defining an `QC.Arbitrary` for the values to be randomly generated. In this case, it's the type `Value`:

```haskell
-- | 'QC.Arbitrary' instance for 'LedgerValue.Value'.
--     This instance generates random 'LedgerValue.Value' values.
--     It randomly pairs 'LedgerValue.CurrencySymbol' with 'LedgerValue.TokenName'
--   and associates them with random Integer values to form a mock map.
instance QC.Arbitrary LedgerValue.Value where
    arbitrary = do
        cs <- QC.arbitrary
        tn <- QC.arbitrary
        val <- QC.arbitrary :: QC.Gen Integer
        let mockMap = TxAssocMap.fromList [(cs, TxAssocMap.singleton tn val)]
        return $ LedgerValue.Value mockMap
```

However, a challenge arises: QuickCheck already has a defined `QC.Arbitrary` for the `Integer` type (hence `arbitrary :: QC.Gen Integer` for `val`), but it lacks a definition for the `PlutusTx.Builtins.Internals.ByteString` type, which is how both `TokenName` and `CurrencySymbol` are represented.

To address this, we define an instance of `QC.Arbitrary` for this type:

```haskell
-- | 'QC.Arbitrary' instance for 'TxBuiltins.BuiltinByteString'.
--   This instance generates random 'TxBuiltins.BuiltinByteString' values.
instance QC.Arbitrary TxBuiltins.BuiltinByteString where
    arbitrary = TxBuiltinsClass.toBuiltin <$> (QC.arbitrary :: QC.Gen BS.ByteString)
```

As with Integers, QuickCheck has a predefined instance for the standard `ByteString` type from Haskell's library. So, we call `QC.Gen BS.ByteString` and convert it to `BuiltinByteString` using `TxBuiltinsClass.toBuiltin`.

## Testing the Contracts

Property testing a function as complex as a validator requires selecting some specific scenario (usually given by some redeemer constructor) and thinking a strategy specific to it.

For instance, let's say we want to check that when updating the Protocol datum (redeemer `ValidatorRedeemerDatumUpdate`) the UTxO value should not change (if it _does_ change, we _must_ get an error). Then our testing strategy in this case would be to make "small random changes" in the Protocol input UTxO value, reflecting this changes in the output, and run the validator in this context, with the proper redeemer.

To cover more cases, we can vary the parameters of the Protocol datum. Achieving this is as simple as defining an `QC.Arbitrary` class instance for the datum type, customizing it to get values as required by the validator. Such instance might look like this:

```haskell
-- | Generates an arbitrary 'ProtocolDatumType'.
instance QC.Arbitrary ProtocolDatumType where
    arbitrary =
        ProtocolDatumType
            <$> (return 2 :: QC.Gen Integer) -- pdProtocolFactoryVersion
            <*> QC.arbitrary -- pdScriptPolicyID_CS
            <*> QC.arbitrary -- pdScriptValidator_Hash
            <*> QC.arbitrary -- pdOraclePaymentPubKey
            <*> ( do
                    admins <- QC.listOf1 arbitrary :: QC.Gen [LedgerApiV2.PubKeyHash]
                    return $ sort admins -- pdAdmins
                )
            <*> (do sort <$> QC.arbitrary) -- pdFundCategories
            <*> QC.arbitrary -- pdFundLifeTime
            <*> QC.arbitrary -- pdRequiredMAYZForSwapOffers
            <*> QC.arbitrary -- pdRequiredMAYZForBuyOrders
            <*> QC.arbitrary -- pdCommissionFund_PerYear_InBPx1e3
            <*> QC.arbitrary -- pdCommissionSwapOffer_InBPx1e3
            <*> QC.arbitrary -- pdCommissionBuyOrder_InBPx1e3
            <*> QC.arbitrary -- pdShare_InBPx1e2_Protocol
            <*> QC.arbitrary -- pdShare_InBPx1e2_Delegators
            <*> QC.arbitrary -- pdShare_InBPx1e2_Managers
            <*> (do sort <$> QC.arbitrary) -- pdDelegatorsAdmins
            <*> QC.arbitrary -- pdMinADA
```

In this case we are using the QuickCheck function `QC.listOf1` to get non-empty lists for the admins, and we are sorting some random generated lists.

Of course we need to specify what "small random changes" in value means. To difine this, we usually follow this steps:
- If there are `n` different assets present in the value, select a random integer `i` between `0` and `n`.
- If `0 <= i < n`, use `i` to select one of the assets.
    - If the asset selected is not an NFT, vary its value randomly within 10%.
    - If the asset is an NFT, then randomly add `1` or `-1` to the value.
- If `i == n`, generate a new random asset. In this case we use a random generated positive integer for the value.

This rules are reflected in a definition like the following:

```haskell
-- | Generates an arbitrary LedgerApiV2.singleton value.
instance QC.Arbitrary RandomSingleton where
    arbitrary = do
        randomIndex <- QC.chooseInteger (0, 2)
        let randomValue = case randomIndex of
                0 -> do
                    randomAmount <-
                        QC.chooseInteger (-200_000, 200_000)
                            `QC.suchThat` (/= 0)
                    return $ LedgerAda.lovelaceValueOf randomAmount
                1 -> do
                    randomAmount <- QC.chooseInteger (-1, 1) `QC.suchThat` (/= 0)
                    return $ LedgerApiV2.singleton exampleCS T.protocolID_TN randomAmount
                _ -> do
                    randomAmount <- QC.arbitrary `QC.suchThat` (/= 0)
                    randomCS <- QC.arbitrary
                    randomTN <- QC.arbitrary
                    return $ LedgerApiV2.singleton randomCS randomTN $ abs randomAmount
        RandomSingleton <$> randomValue
```

Finally, we run our evaluateScriptValidatorEX. Since it returns a list containing the error string raised by the validator (if any), we check that this list isn't empty:

```haskell
let result =
        evaluateScriptValidatorEX
            protocolValidator
            protocolDatum
            updateProtocolRedeemer
            ctx
in  not $ null result
```
