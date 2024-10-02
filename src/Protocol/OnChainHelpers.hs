{-# LANGUAGE RecordWildCards #-}

--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

module Protocol.OnChainHelpers where

--------------------------------------------------------------------------------2
-- External Imports
--------------------------------------------------------------------------------2

import qualified Plutus.V2.Ledger.Api                as LedgerApiV2
import           PlutusTx.Prelude                    hiding (unless)

--------------------------------------------------------------------------------2
-- Internal Imports
--------------------------------------------------------------------------------2

import qualified Generic.OnChainHelpers              as OnChainHelpers
import qualified Protocol.Types                      as T

--------------------------------------------------------------------------------2
-- Module
--------------------------------------------------------------------------------2

{-# INLINEABLE oracleReIdxDataToBBS #-}
oracleReIdxDataToBBS :: T.OracleReIdx_Data -> BuiltinByteString
oracleReIdxDataToBBS T.OracleReIdx_Data {..} =
    let
        valueToBBS :: (LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer) -> BuiltinByteString
        valueToBBS (cs, tn, amt) = LedgerApiV2.unCurrencySymbol cs <> LedgerApiV2.unTokenName tn <> OnChainHelpers.intToBBS amt

        investUnitToBBS :: T.InvestUnit -> BuiltinByteString
        investUnitToBBS investUnit  = foldl (<>) emptyByteString (valueToBBS <$> T.iuValues investUnit)

        oridTokensPriceADABBS = investUnitToBBS oridTokensPriceADA
        oridTimeBBS = OnChainHelpers.pOSIXTimeToBBS oridTime

    in  oridTokensPriceADABBS <> oridTimeBBS


{-# INLINEABLE oracleDataToBBS #-}
oracleDataToBBS :: T.Oracle_Data -> BuiltinByteString
oracleDataToBBS T.Oracle_Data {..} =
    let
        valueToBBS :: (LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer) -> BuiltinByteString
        valueToBBS (cs, tn, amt) = LedgerApiV2.unCurrencySymbol cs <> LedgerApiV2.unTokenName tn <> OnChainHelpers.intToBBS amt

        investUnitToBBS :: T.InvestUnit -> BuiltinByteString
        investUnitToBBS investUnit  = foldl (<>) emptyByteString (valueToBBS <$> T.iuValues investUnit)

        oridTokensPriceADABBS = investUnitToBBS odFTPriceADA1xe6
        oridTimeBBS = OnChainHelpers.pOSIXTimeToBBS odTime

    in  oridTokensPriceADABBS <> oridTimeBBS


{-# INLINEABLE getDecimalsInInvestUnit #-}
getDecimalsInInvestUnit :: [T.InvestUnitToken] -> Integer
getDecimalsInInvestUnit tokens = go tokens 1
    where
        ------------------
        go [] acc = acc
        go ((_, _, amount):xs) acc =
            let accNew = max acc (dividedBy amount)
            in go xs accNew
        ------------------
        dividedBy amount
            | amount `remainder` 100 == 0 = 1
            | amount `remainder` 10 == 0 = 10
            | otherwise = 100

--------------------------------------------------------------------------------2

