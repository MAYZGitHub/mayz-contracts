{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE RankNTypes        #-}
--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2
{- |
Module      : TestUtils.QuickCheckGen.QuickCheckGen
Description :
-}
module TestUtils.QuickCheckGen.QuickCheckGen where

-- Non-IOG imports
import qualified Control.Monad              as ControlMonad (replicateM)
import qualified Data.ByteString            as BS
import qualified Data.Word                  as DataWord (Word8)
import           Prelude                    hiding (fromInteger, map, zip, ($), (*))
import qualified Test.QuickCheck            as QC

-- IOG imports
import qualified Ledger.Ada                 as LedgerAda
import qualified Ledger.Address             as LedgerAddress
import qualified Ledger.Crypto              as LedgerCrypto
import qualified Plutus.V1.Ledger.Value     as LedgerValue
import qualified Plutus.V2.Ledger.Api       as LedgerApiV2
import qualified PlutusTx.AssocMap          as TxAssocMap
import qualified PlutusTx.Builtins.Class    as TxBuiltinsClass
import qualified PlutusTx.Builtins.Internal as TxBuiltins
import           PlutusTx.Prelude           (fromInteger, map, unsafeRatio, ($), (*))
import qualified PlutusTx.Ratio             as TxRatio

-- Project imports

import qualified Generic.OffChainHelpers    as OffChainHelpers
import qualified Generic.OnChainHelpers     as OnChainHelpers

-----------------------------

-- Define the new type
newtype IntegerPlusZero
    = IntegerPlusZero { getIntegerPlusZero :: Integer }
    deriving (Show)
newtype IntegerMinusZero
    = IntegerMinusZero { getIntegerMinusZero :: Integer }
    deriving (Show)

-- Implement the QC.Arbitrary instance
instance QC.Arbitrary IntegerPlusZero where
    arbitrary :: QC.Gen IntegerPlusZero
    arbitrary = IntegerPlusZero <$> (QC.arbitrary `QC.suchThat` (> 0))

instance QC.Arbitrary IntegerMinusZero where
    arbitrary :: QC.Gen IntegerMinusZero
    arbitrary = IntegerMinusZero <$> (QC.arbitrary `QC.suchThat` (< 0))

-- | Generates random 'TxBuiltins.BuiltinByteString' values.
instance QC.Arbitrary TxBuiltins.BuiltinByteString where
    arbitrary = gen56BuiltinByteString

-----------------------------


-- | Generates random 'LedgerValue.CurrencySymbol' values.
instance QC.Arbitrary LedgerValue.CurrencySymbol where
    arbitrary = genCurrencySymbol

-- | Generates random 'LedgerValue.TokenName' values.
instance QC.Arbitrary LedgerValue.TokenName where
    arbitrary = genTokenName


instance QC.Arbitrary LedgerApiV2.ValidatorHash where
    arbitrary = LedgerApiV2.ValidatorHash <$> gen56BuiltinByteString

{- | Generates an arbitrary 'PaymentPubKey'.
     'PaymentPubKey' wraps a public key used for payment purposes.
     This instance generates a 'PaymentPubKey' using a random 'PubKey'.
-}
instance QC.Arbitrary LedgerAddress.PaymentPubKey where
    arbitrary = LedgerAddress.PaymentPubKey . LedgerCrypto.PubKey . LedgerApiV2.LedgerBytes <$> gen56BuiltinByteString

{- | Generates an arbitrary 'PubKeyHash'.
     'PubKeyHash' is the hash of a public key.
-}
instance QC.Arbitrary LedgerApiV2.PubKeyHash where
    arbitrary :: QC.Gen LedgerApiV2.PubKeyHash
    arbitrary = LedgerApiV2.PubKeyHash <$> gen56BuiltinByteString

{- | Generates an arbitrary 'POSIXTime'.
     'POSIXTime' represents time in the Plutus ledger.
-}
instance QC.Arbitrary LedgerApiV2.POSIXTime where
    arbitrary :: QC.Gen LedgerApiV2.POSIXTime
    arbitrary = LedgerApiV2.POSIXTime <$> (QC.arbitrary :: QC.Gen Integer)

{- | Generates random 'LedgerValue.Value' values.
     It randomly pairs 'LedgerValue.CurrencySymbol' with 'LedgerValue.TokenName'
     and associates them with random Integer values to form a mock map.
-}
instance QC.Arbitrary LedgerValue.Value where
    arbitrary = do
        numEntries <- QC.choose (1, 10)
        entries <- ControlMonad.replicateM numEntries $ do
            cs <- QC.arbitrary :: QC.Gen LedgerApiV2.CurrencySymbol
            tn <- QC.arbitrary :: QC.Gen LedgerApiV2.TokenName
            val <- QC.arbitrary :: QC.Gen Integer
            return (cs, tn, val)
        let mockMap = buildValueMap entries
        return $ LedgerValue.Value mockMap

-- | Generates an arbitrary UTxO value.
instance QC.Arbitrary LedgerApiV2.TxOut where
    arbitrary = do
        pubKeyHash <- QC.arbitrary
        value <- QC.arbitrary
        return $
            LedgerApiV2.TxOut
                (LedgerAddress.pubKeyHashAddress (LedgerAddress.PaymentPubKeyHash pubKeyHash) Nothing)
                value
                LedgerApiV2.NoOutputDatum
                Nothing

-- | Generates an arbitrary LedgerApiV2.TxOutRef value.
instance QC.Arbitrary LedgerApiV2.TxOutRef where
    arbitrary = do
        refId <- gen56BuiltinByteString
        refIdx <- QC.arbitrary :: QC.Gen Integer
        return $
            LedgerApiV2.TxOutRef
                { LedgerApiV2.txOutRefId = LedgerApiV2.TxId refId
                , LedgerApiV2.txOutRefIdx = refIdx
                }

-- | Generates an arbitrary Script LedgerApiV2.Address.
instance QC.Arbitrary LedgerApiV2.Address where
    arbitrary = do
        validatorHash <- QC.arbitrary :: QC.Gen LedgerApiV2.ValidatorHash
        return $
            LedgerApiV2.Address
                { LedgerApiV2.addressCredential = LedgerApiV2.ScriptCredential validatorHash
                , LedgerApiV2.addressStakingCredential = Nothing
                }

-----------------------------------------------

-- | Singleton value.
newtype RandomSingleton
    = RandomSingleton { getRandomSingleton :: LedgerApiV2.Value }
    deriving (Show)

-- | Generates an arbitrary LedgerApiV2.singleton value.
instance QC.Arbitrary RandomSingleton where
    -- arbitrary = arbitraryInAppM $ do
    arbitrary :: QC.Gen RandomSingleton
    arbitrary =  do
            -- params <- testParams2
            randomIndex <- QC.chooseInteger (0, 2)
            let randomValue = case randomIndex of
                    0 -> do
                        -- TODO: no tiene el valor abs el value final
                        randomAmount <-
                            QC.chooseInteger (-200_000, 200_000)
                                `QC.suchThat` (/= 0)
                        return $ LedgerAda.lovelaceValueOf randomAmount
                    1 -> do
                        randomAmount <- QC.chooseInteger (-1, 1) `QC.suchThat` (/= 0)
                        randomCS <- QC.arbitrary :: QC.Gen LedgerApiV2.CurrencySymbol
                        randomTN <- QC.arbitrary :: QC.Gen LedgerApiV2.TokenName
                        return $ LedgerApiV2.singleton randomCS randomTN $ abs randomAmount
                    _ -> do
                        randomAmount <- QC.arbitrary `QC.suchThat` (/= 0)
                        randomCS <- QC.arbitrary :: QC.Gen LedgerApiV2.CurrencySymbol
                        randomTN <- QC.arbitrary    :: QC.Gen LedgerApiV2.TokenName
                        return $ LedgerApiV2.singleton randomCS randomTN $ abs randomAmount
            RandomSingleton <$> randomValue

---------------------------------------------------------------------------------------

-- Generator for positive Integer
genPositiveInteger :: QC.Gen Integer
genPositiveInteger = QC.arbitrary `QC.suchThat` (> 0)

---------------------------------------------------------------------------------------

genHexDigit :: QC.Gen Char
genHexDigit = QC.elements (['0'..'9'] ++ ['a'..'f'])

-- Generator for a 56-character hexadecimal string
genHexString56 :: QC.Gen String
genHexString56 = QC.vectorOf 56 genHexDigit

gen56BuiltinByteString :: QC.Gen TxBuiltins.BuiltinByteString
gen56BuiltinByteString = TxBuiltinsClass.toBuiltin . BS.pack <$> QC.vectorOf 28 (QC.choose (0, 255) :: QC.Gen DataWord.Word8)

-- Generator for CurrencySymbol (assuming it's a ByteString in hex format)
genCurrencySymbol :: QC.Gen LedgerApiV2.CurrencySymbol
genCurrencySymbol = LedgerApiV2.CurrencySymbol . OffChainHelpers.stringToBuiltinByteString <$>  genHexString56

-- Generator for TokenName (assuming it's a ByteString)
genTokenName :: QC.Gen LedgerApiV2.TokenName
genTokenName = do
    length' <- QC.choose (1, 100)
    hexString <- QC.vectorOf length' genHexDigit
    return $ LedgerApiV2.TokenName $ OffChainHelpers.stringToBuiltinByteString hexString

---------------------------------------------------------------------------------------

-- | 'buildMap' constructs an associative map from a list of triples.
buildValueMap ::
    [(LedgerValue.CurrencySymbol, LedgerValue.TokenName, Integer)] ->
    TxAssocMap.Map
        LedgerValue.CurrencySymbol
        (TxAssocMap.Map LedgerValue.TokenName Integer)
buildValueMap = foldr insertTriple TxAssocMap.empty
    where
        insertTriple (cs, tn, val) outerMap =
            let innerMap =
                    TxAssocMap.lookup
                        cs
                        outerMap
                        `orDefault` TxAssocMap.empty
                updatedInnerMap = TxAssocMap.insert tn val innerMap
             in TxAssocMap.insert cs updatedInnerMap outerMap

        orDefault (Just v) _ = v
        orDefault Nothing d  = d

---------------------------------------------------------------------------------------

{- | Randomly varies a value, following these steps:
     - Adds a random asset with a random positive amount.
     - Selects a random sublist of assets to vary.
     - For amounts greater than 1, varies the amount within 10%.
     - For amounts equal to 1 (like an NFT), randomly adds 1, 0 or -1.
-}
varyValue :: LedgerApiV2.Value -> QC.Gen LedgerApiV2.Value
varyValue val = do
    randomCS <- QC.arbitrary :: QC.Gen LedgerValue.CurrencySymbol
    randomTN <- QC.arbitrary :: QC.Gen LedgerValue.TokenName
    randomAmount <- QC.arbitrary :: QC.Gen Integer
    let flatValue = LedgerValue.flattenValue val
        flatValue' = (randomCS, randomTN, abs randomAmount) : flatValue
    randomSubList <- QC.sublistOf flatValue'
    randomPercentage <- QC.chooseInteger (-100, 100)
    randomInt <- QC.chooseInteger (-1, 1)
    let randomSubList' =
            map
            (\(cs, tn, am) ->
                if am == 1
                    then (cs, tn, randomInt)
                    else (cs, tn,
                            TxRatio.round $
                            unsafeRatio randomPercentage 1000 * fromInteger am
                         )
            )
            randomSubList
    return (OnChainHelpers.flattenValueToValue randomSubList' <> val)

---------------------------------------------------------------------------------------
