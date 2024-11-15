{-# LANGUAGE RecordWildCards #-}

--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

module Protocol.OnChainHelpers where

--------------------------------------------------------------------------------2
-- External Imports
--------------------------------------------------------------------------------2

import qualified Plutus.V2.Ledger.Api   as LedgerApiV2
import           PlutusTx.Prelude       hiding (unless)

--------------------------------------------------------------------------------2
-- Internal Imports
--------------------------------------------------------------------------------2

import qualified Generic.OnChainHelpers as OnChainHelpers
import qualified Protocol.Types         as T
import qualified Ledger

--------------------------------------------------------------------------------2
-- Module
--------------------------------------------------------------------------------2

{-# INLINEABLE oracleReIdxDataToBBS #-}
oracleReIdxDataToBBS :: T.OracleReIdx_Data -> BuiltinByteString
oracleReIdxDataToBBS T.OracleReIdx_Data {..} =
    let
        valueToBBS :: (LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer) -> BuiltinByteString
        valueToBBS (!cs, !tn, !amt) = LedgerApiV2.unCurrencySymbol cs <> LedgerApiV2.unTokenName tn <> OnChainHelpers.intToBBS amt

        investUnitToBBS :: T.InvestUnit -> BuiltinByteString
        investUnitToBBS !investUnit  = foldl (<>) emptyByteString (valueToBBS <$> T.iuValues investUnit)

        !oridTokensPriceADABBS = investUnitToBBS oridTokensPriceADA
        !oridTimeBBS = OnChainHelpers.pOSIXTimeToBBS oridTime

    in  oridTokensPriceADABBS <> oridTimeBBS


{-# INLINEABLE oracleDataToBBS #-}
oracleDataToBBS :: T.Oracle_Data -> BuiltinByteString
oracleDataToBBS T.Oracle_Data {..} =
    let
        valueToBBS :: (LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer) -> BuiltinByteString
        valueToBBS (!cs, !tn, !amt) = LedgerApiV2.unCurrencySymbol cs <> LedgerApiV2.unTokenName tn <> OnChainHelpers.intToBBS amt

        investUnitToBBS :: T.InvestUnit -> BuiltinByteString
        investUnitToBBS !investUnit  = foldl (<>) emptyByteString (valueToBBS <$> T.iuValues investUnit)

        !oridTokensPriceADABBS = investUnitToBBS odFTPriceADA1xe6
        !oridTimeBBS = OnChainHelpers.pOSIXTimeToBBS odTime

    in  oridTokensPriceADABBS <> oridTimeBBS

{-# INLINEABLE isCorrect_Oracle_Signature #-}
-- NOTE: very important not to use bang patterns here, because it will cause a runtime error when compiling plutus template of smart contracts
isCorrect_Oracle_Signature :: LedgerApiV2.BuiltinByteString -> Ledger.PaymentPubKey -> Ledger.Signature -> Bool
isCorrect_Oracle_Signature priceData oraclePaymentPubKey oracle_Signature =
    let
        ------------------
        checkSignature ::
            Ledger.PaymentPubKey ->
            -- \^ The public key of the signatory
            LedgerApiV2.BuiltinByteString ->
            -- \^ The message
            Ledger.Signature ->
            -- \^ The signed message
            Bool
        checkSignature paymentPubKey signedMsgBBS signature =
            let
                pubKey = Ledger.unPaymentPubKey paymentPubKey
                lb = Ledger.getPubKey pubKey
                bbs = LedgerApiV2.getLedgerBytes lb
                sig = Ledger.getSignature signature
            in
                verifyEd25519Signature bbs signedMsgBBS sig
    in
        ------------------
        checkSignature oraclePaymentPubKey priceData oracle_Signature

{-# INLINEABLE isCorrect_Oracle_InRangeTime #-}
isCorrect_Oracle_InRangeTime :: LedgerApiV2.TxInfo -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Bool
isCorrect_Oracle_InRangeTime !info !oracle_Time !oracleData_Valid_Time =
    case Ledger.ivFrom (LedgerApiV2.txInfoValidRange info) of
        Ledger.LowerBound (Ledger.Finite txStartTime) _ -> 
            txStartTime - oracle_Time <= oracleData_Valid_Time
        _ -> traceError "Interval has no lower bound"

--------------------------------------------------------------------------------

{-# INLINEABLE getDecimalsInInvestUnit #-}
getDecimalsInInvestUnit :: [T.InvestUnitToken] -> Integer
getDecimalsInInvestUnit !tokens = go tokens 1
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