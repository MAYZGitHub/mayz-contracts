{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module TestUtils.Automatic.ParamsGeneratorsMAYZ where

--------------------------------------------------------------------------------

-- Non-IOG imports

import qualified Data.Data                                 as Data
import qualified Data.Map                                  as DataMap
import           Prelude                                   as P hiding ((<>))
import qualified Test.QuickCheck                           as QC

-- IOG imports
import qualified Plutus.V2.Ledger.Api                      as LedgerApiV2

-- Project imports

import qualified Generic.OnChainHelpers                    as OnChainHelpers
import qualified Protocol.Fund.Helpers                     as FundHelpers
import qualified Protocol.OnChainHelpers                   as OnChainHelpers
import qualified Protocol.Types                            as T
import           TestUtils.Automatic.Types
import           TestUtils.Constants
import           TestUtils.Helpers
import           TestUtils.QuickCheckGen.QuickCheckGenMAYZ
import           TestUtils.Types


----------------------------------------------------------------------------------------
-- Parameter generator for InvestUnit
investUnitParam :: String -> Integer -> Integer -> TxParamGenerator
investUnitParam name qtyMin qtyMax = TxParamGenerator (const $ genInvestUnitWithGranularity qtyMin qtyMax False False) name

-- New dependent parameter for InvestUnit
dependentInvestUnitParam :: String -> Bool -> String -> TxParamGenerator
dependentInvestUnitParam name useForcedGranularity dependsOnNameParamForQty =
    TxParamGenerator
        (\accum -> do
            case DataMap.lookup dependsOnNameParamForQty accum of
                Just (TxParam _ quantity) ->
                    case Data.cast quantity of
                        Just (qty :: Integer) -> do
                                !_ <- debugTraceIf swTraceTxParamsGenererator ("InvestUnit - quantity: " ++ show qty)
                                genInvestUnitWithGranularity qty qty useForcedGranularity False
                        Nothing               -> error $ "Invalid type for " ++ dependsOnNameParamForQty
                Nothing -> error $ "Required parameter '" ++ dependsOnNameParamForQty ++ "' not found"
        )
        name

dependentInvestUnitParamNoDecimals :: String -> String -> TxParamGenerator
dependentInvestUnitParamNoDecimals name dependsOnNameParamForQty =
    TxParamGenerator
        (\accum -> do
            case DataMap.lookup dependsOnNameParamForQty accum of
                Just (TxParam _ quantity) ->
                    case Data.cast quantity of
                        Just (qty :: Integer) -> genInvestUnitWithGranularity qty qty False True
                        Nothing               -> error $ "Invalid type for " ++ dependsOnNameParamForQty
                Nothing -> error $ "Required parameter '" ++ dependsOnNameParamForQty ++ "' not found"
        )
        name

-- Dependent parameter for deposit amount
dependentDepositParam :: String -> Integer -> Integer -> Bool -> String -> TxParamGenerator
dependentDepositParam name minDeposit maxDeposit validGranularity dependsOnNameParamForIUTokens =
    TxParamGenerator
            (\accum -> do
                case DataMap.lookup dependsOnNameParamForIUTokens accum of
                    Just (TxParam _ investUnit) ->
                        case Data.cast investUnit of
                            Just (investUnit' :: T.InvestUnit) -> do
                                let investUnitTokens = T.iuValues investUnit'
                                -- Determine the granularity of the InvestUnit
                                let investUnit_Granularity = OnChainHelpers.getDecimalsInInvestUnit investUnitTokens
                                if validGranularity then
                                    -- Generate a valid deposit amount
                                    -- It will be divisible by the InvestUnit's granularity
                                    do
                                        amount <- QC.choose (minDeposit `div` investUnit_Granularity, maxDeposit `div` investUnit_Granularity)
                                        return $ amount * investUnit_Granularity
                                else
                                    -- Generate an invalid deposit amount
                                    -- It will not be divisible by the InvestUnit's granularity
                                    do
                                        baseAmount <- QC.choose (minDeposit `div` investUnit_Granularity, maxDeposit `div` investUnit_Granularity)
                                        invalidOffset <- QC.choose (1, investUnit_Granularity - 1)
                                        return $ (baseAmount * investUnit_Granularity) + invalidOffset
                            Nothing -> error "Invalid type for investUnitTokens"
                    Nothing -> error "Required parameter 'investUnitTokens' not found"
            )
            name

dependentWithdrawParam :: String -> Integer -> Integer -> Bool -> Bool -> String -> String -> String -> String -> String -> String -> TxParamGenerator
dependentWithdrawParam name minWithdraw maxWithdraw validGranularity useValidAmount dependsOnNameParamForIUTokens dependsOnNameParamForDeposit dependsOnNameParamForBeginDate dependsOnNameParamForDeadlineDate dependsOnNameParamForDepositDate dependsOnNameParamForCommission =
    TxParamGenerator
            (\accum -> do
                case (DataMap.lookup dependsOnNameParamForIUTokens accum,
                      DataMap.lookup dependsOnNameParamForDeposit accum,
                      DataMap.lookup dependsOnNameParamForBeginDate accum,
                      DataMap.lookup dependsOnNameParamForDeadlineDate accum,
                      DataMap.lookup dependsOnNameParamForDepositDate accum,
                      DataMap.lookup dependsOnNameParamForCommission accum) of
                    (Just (TxParam _ investUnit),
                     Just (TxParam _ deposit),
                     Just (TxParam _ beginDate),
                     Just (TxParam _ deadlineDate),
                     Just (TxParam _ depositDate),
                     Just (TxParam _ commission)) ->
                        case (Data.cast investUnit, Data.cast deposit, Data.cast beginDate, Data.cast deadlineDate, Data.cast depositDate, Data.cast commission) of
                            (Just (investUnit' :: T.InvestUnit),
                             Just (deposit' :: Integer),
                             Just (beginDate' :: LedgerApiV2.POSIXTime),
                             Just (deadlineDate' :: LedgerApiV2.POSIXTime),
                             Just (depositDate' :: LedgerApiV2.POSIXTime),
                             Just (commission' :: Integer)) -> do
                                let investUnitTokens = T.iuValues investUnit'
                                let investUnit_Granularity = OnChainHelpers.getDecimalsInInvestUnit investUnitTokens
                                let monthsRemainingPlusOne = FundHelpers.getRemainingMonths deadlineDate' beginDate' + 1
                                let den = 120_000_000
                                let commissions_Table_Numerator_1e6 = [OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator $ OnChainHelpers.powRational (den - commission') den month | month <- [0 .. monthsRemainingPlusOne]]
                                let (userFT, _commissionsFT, _rate) = FundHelpers.calculateDepositCommissionsUsingMonths commissions_Table_Numerator_1e6 deadlineDate' depositDate' deposit'
                                !_ <- debugTraceIf swTraceTxParamsGenererator ("useValidAmount - validGranularity: " ++ show (useValidAmount, validGranularity, investUnit_Granularity, userFT, minWithdraw, maxWithdraw))
                                if useValidAmount then
                                    if validGranularity then
                                        do
                                            amount <- QC.choose (minWithdraw `div` investUnit_Granularity, min maxWithdraw userFT `div` investUnit_Granularity)
                                            !_ <- debugTraceIf swTraceTxParamsGenererator ("withdraw amount: " ++ show (amount * investUnit_Granularity))
                                            return $ amount * investUnit_Granularity
                                    else
                                        do
                                            baseAmount <- QC.choose (minWithdraw `div` investUnit_Granularity, min maxWithdraw userFT `div` investUnit_Granularity)
                                            invalidOffset <- QC.choose (1, investUnit_Granularity - 1)
                                            debugTraceIf_ swTraceTxParamsGenererator ("baseAmount: " ++ show baseAmount ++ ", invalidOffset: " ++ show invalidOffset ++ ", investUnit_Granularity: " ++ show investUnit_Granularity ++ ", amount: " ++ show ((baseAmount * investUnit_Granularity) + invalidOffset)) $
                                                return $ (baseAmount * investUnit_Granularity) + invalidOffset
                                else
                                    do
                                        amount <- QC.choose ((userFT + minWithdraw) `div` investUnit_Granularity, (userFT + maxWithdraw) `div` investUnit_Granularity)
                                        debugTraceIf_ swTraceTxParamsGenererator ("withdraw amount: " ++ show (amount * investUnit_Granularity)) $
                                            return $ amount * investUnit_Granularity
                            _ -> error "Invalid type for parameters"
                    _ -> error "Required parameters not found"
            )
            name

-----------------------------
