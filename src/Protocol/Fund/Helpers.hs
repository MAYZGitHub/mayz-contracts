{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

module Protocol.Fund.Helpers where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Plutus.V2.Ledger.Api        as LedgerApiV2
import           PlutusTx.Prelude            hiding (unless)
import qualified PlutusTx.Ratio              as TxRatio

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.OnChainHelpers      as OnChainHelpers
import qualified Generic.Types               as T
import qualified Protocol.Fund.Holding.Types as FundHoldingT
import qualified Protocol.Fund.Types         as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_Fund_Datum_With_NormalChanges #-}
mkUpdated_Fund_Datum_With_NormalChanges :: T.FundDatumType -> [T.WalletPaymentPKH] -> LedgerApiV2.CurrencySymbol -> T.FundDatumType
mkUpdated_Fund_Datum_With_NormalChanges !fundDatum_In !admins !tokenAdminPolicy_CS =
    fundDatum_In { T.fdAdmins = admins, T.fdTokenAdminPolicy_CS = tokenAdminPolicy_CS }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_Fund_Datum_With_MinADAChanged #-}
mkUpdated_Fund_Datum_With_MinADAChanged :: T.FundDatumType -> Integer -> T.FundDatumType
mkUpdated_Fund_Datum_With_MinADAChanged !fundDatum_In !newMinADA = fundDatum_In { T.fdMinADA = newMinADA }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_Fund_Datum_With_ClosedAt #-}
mkUpdated_Fund_Datum_With_ClosedAt :: T.FundDatumType -> LedgerApiV2.POSIXTime -> T.FundDatumType
mkUpdated_Fund_Datum_With_ClosedAt !fundDatum_In !closedAt = fundDatum_In { T.fdClosedAt = Just closedAt }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_Fund_Datum_With_HoldingAdded #-}
mkUpdated_Fund_Datum_With_HoldingAdded :: T.FundDatumType -> T.FundDatumType
mkUpdated_Fund_Datum_With_HoldingAdded !fundDatum_In =
    fundDatum_In { T.fdHoldingsCount = T.fdHoldingsCount fundDatum_In + 1, T.fdHoldingsIndex = T.fdHoldingsIndex fundDatum_In + 1 }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_Fund_Datum_With_HoldingDeleted #-}
mkUpdated_Fund_Datum_With_HoldingDeleted :: T.FundDatumType -> T.FundDatumType
mkUpdated_Fund_Datum_With_HoldingDeleted !fundDatum_In =
    fundDatum_In { T.fdHoldingsCount = T.fdHoldingsCount fundDatum_In - 1 }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_FundHolding_Datum_With_Deposit #-}
mkUpdated_FundHolding_Datum_With_Deposit :: FundHoldingT.FundHoldingDatumType -> Integer  -> Integer  -> Integer -> Integer -> FundHoldingT.FundHoldingDatumType
mkUpdated_FundHolding_Datum_With_Deposit !fundHoldingDatum_In !mintingFT !_deliverFT !commissionsPayed !commissions_FT_Rate1e6_PerMonth =
    fundHoldingDatum_In {
        FundHoldingT.hdSubtotal_FT_Minted_Accumulated = FundHoldingT.hdSubtotal_FT_Minted_Accumulated fundHoldingDatum_In + mintingFT
        , FundHoldingT.hdSubtotal_FT_Minted = FundHoldingT.hdSubtotal_FT_Minted fundHoldingDatum_In + mintingFT
        , FundHoldingT.hdSubtotal_FT_Commissions = FundHoldingT.hdSubtotal_FT_Commissions fundHoldingDatum_In + commissionsPayed
        , FundHoldingT.hdSubtotal_FT_Commissions_Rate1e6_PerMonth = FundHoldingT.hdSubtotal_FT_Commissions_Rate1e6_PerMonth fundHoldingDatum_In + commissions_FT_Rate1e6_PerMonth
    }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_FundHolding_Datum_With_Withdraw #-}
mkUpdated_FundHolding_Datum_With_Withdraw :: FundHoldingT.FundHoldingDatumType ->Integer  -> Integer  -> Integer  -> FundHoldingT.FundHoldingDatumType
mkUpdated_FundHolding_Datum_With_Withdraw !fundHoldingDatum_In !withdraw !commissionsForUserFTToGetBack !commissions_FT_Rate1e6_PerMonth =
    fundHoldingDatum_In {
        FundHoldingT.hdSubtotal_FT_Minted = FundHoldingT.hdSubtotal_FT_Minted fundHoldingDatum_In - withdraw - commissionsForUserFTToGetBack
        , FundHoldingT.hdSubtotal_FT_Commissions = FundHoldingT.hdSubtotal_FT_Commissions fundHoldingDatum_In - commissionsForUserFTToGetBack
        , FundHoldingT.hdSubtotal_FT_Commissions_Rate1e6_PerMonth = FundHoldingT.hdSubtotal_FT_Commissions_Rate1e6_PerMonth fundHoldingDatum_In - commissions_FT_Rate1e6_PerMonth
    }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_FundHolding_Datum_With_Collect_Protocol_Commission #-}
mkUpdated_FundHolding_Datum_With_Collect_Protocol_Commission :: FundHoldingT.FundHoldingDatumType ->Integer  -> FundHoldingT.FundHoldingDatumType
mkUpdated_FundHolding_Datum_With_Collect_Protocol_Commission !fundHoldingDatum_In !withdraw  =
    fundHoldingDatum_In {
        FundHoldingT.hdSubtotal_FT_Commissions = FundHoldingT.hdSubtotal_FT_Commissions fundHoldingDatum_In - withdraw
        , FundHoldingT.hdSubtotal_FT_Commissions_Collected_Protocol = FundHoldingT.hdSubtotal_FT_Commissions_Collected_Protocol fundHoldingDatum_In + withdraw
    }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_FundHolding_Datum_With_Collect_Managers_Commission #-}
mkUpdated_FundHolding_Datum_With_Collect_Managers_Commission :: FundHoldingT.FundHoldingDatumType -> Integer  -> FundHoldingT.FundHoldingDatumType
mkUpdated_FundHolding_Datum_With_Collect_Managers_Commission !fundHoldingDatum_In !withdraw  =
    fundHoldingDatum_In {
        FundHoldingT.hdSubtotal_FT_Commissions = FundHoldingT.hdSubtotal_FT_Commissions fundHoldingDatum_In - withdraw
        , FundHoldingT.hdSubtotal_FT_Commissions_Collected_Managers = FundHoldingT.hdSubtotal_FT_Commissions_Collected_Managers fundHoldingDatum_In + withdraw
    }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_FundHolding_Datum_With_Collect_Delegators_Commission #-}
mkUpdated_FundHolding_Datum_With_Collect_Delegators_Commission :: FundHoldingT.FundHoldingDatumType ->Integer  -> FundHoldingT.FundHoldingDatumType
mkUpdated_FundHolding_Datum_With_Collect_Delegators_Commission !fundHoldingDatum_In !withdraw  =
    fundHoldingDatum_In {
        FundHoldingT.hdSubtotal_FT_Commissions = FundHoldingT.hdSubtotal_FT_Commissions fundHoldingDatum_In - withdraw
        , FundHoldingT.hdSubtotal_FT_Commissions_Collected_Delegators = FundHoldingT.hdSubtotal_FT_Commissions_Collected_Delegators fundHoldingDatum_In + withdraw
    }

--------------------------------------------------------------------------------

{-# INLINEABLE mkUpdated_FundHolding_Datum_With_MinADAChanged #-}
mkUpdated_FundHolding_Datum_With_MinADAChanged :: FundHoldingT.FundHoldingDatumType -> Integer -> FundHoldingT.FundHoldingDatumType
mkUpdated_FundHolding_Datum_With_MinADAChanged !fundHoldingDatum_In !newMinADA = fundHoldingDatum_In { FundHoldingT.hdMinADA = newMinADA }

{-# INLINEABLE mkUpdated_FundHolding_Datum_With_CommissionsMoved #-}
mkUpdated_FundHolding_Datum_With_CommissionsMoved :: FundHoldingT.FundHoldingDatumType -> Integer ->  Integer -> FundHoldingT.FundHoldingDatumType
mkUpdated_FundHolding_Datum_With_CommissionsMoved !fundHoldingDatum_In !newCommissions !rateFT1x6 = fundHoldingDatum_In
    { FundHoldingT.hdSubtotal_FT_Commissions = newCommissions,
        FundHoldingT.hdSubtotal_FT_Commissions_Rate1e6_PerMonth = rateFT1x6
    }

--------------------------------------------------------------------------------2

{-# INLINEABLE getRemainingMonths #-}
getRemainingMonths :: LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer
getRemainingMonths deadline date =
    let
        !msPerMonth = 2_592_000_000 :: Integer -- 1000 * 60 * 60 * 24 * 30
        !msRemaining =  LedgerApiV2.getPOSIXTime $ deadline - date
        !monthsRemaining = msRemaining `divide` msPerMonth
    in if monthsRemaining < 0 then 0 else monthsRemaining

--------------------------------------------------------------------------------2

{-# INLINEABLE calculateDepositCommissionsUsingMonths #-}
calculateDepositCommissionsUsingMonths :: [Integer] -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> (Integer, Integer, Integer)
calculateDepositCommissionsUsingMonths commissionsTable_Numerator1e6 deadline date deposit = (userFT, commissionsFT, commissions_FT_Rate1e6_PerMonth)
  where
    -- las comisiones son por en realidad por año y en basic points BP multiplicados por 1e3 o lo que es igual 10e2 = 1_000
    -- la formula de comisiones es = (1 - comisiones por periodo en porcentaje del 0 al 1) ^ (periodos restantes + 1)
    -- el periodo aqui lo voy a calcular en meses
    -- eso significa que al valor de commissionPerYearInBPx1e3 tengo que dividirlo por
    -- 10e2 para pasarlo a bp
    -- 100 para pasarlo a porcentaje normal del 1 al 100
    -- 100 para pasarlo a porcentaje del 0 al 1
    -- 12 para pasarlo a meses
    -- defino den = 1e3 * 100 * 100 * 12 = 1000 * 100 * 100 * 12 = 120_000_000
    -- commissionesPerMonthPct0to1 = TxRatio.unsafeRatio commissionPerYearInBPx1e3 den
    -- pero en lugar de calcular el rational y leugo volver a separarlo en num y den para llamar a la potencia
    -- voy a calcular primero el 1 - commissionesPerMonthPct0to1 de esta forma:
    -- commissionesToUse = TxRatio.unsafeRatio (den - commissionPerYearInBPx1e3) den
    -- pero este Rational tampoco hace falta crearlo, puedo usar directamente esos num y den para llamar a la potencia
    -- commissionsAcumulated = OnChainHelpers.powRational (den - commissionPerYearInBPx1e3) den (daysRemaining + 1)
    ------------------
    !monthsRemaining = getRemainingMonths deadline date
    ------------------
    -- !den = 120_000_000
    -- !commissionsAcumulated = OnChainHelpers.powRational (den - commissionPerYearInBPx1e3) den (monthsRemaining + 1)
    -- !userFT = TxRatio.truncate (commissionsAcumulated * TxRatio.fromInteger deposit)
    ------------------
    -- Estoy usando una tabla de comisiones pre calculada
    -- la tabla tiene en cada posicion el valor de las comisiones acumuladas para ese cantidad de mes restantes
    -- ademas esta guardada como enteros x 1e6, por lo que para obtener el valor real hay que dividir por 1e6
    -- aprovechando que es entero, primero calculo la multiplicacion por deposit, que es entero, y luego lo divido por 1e6
    ------------------
    !commissionsAcumulated_Numerator1e6 = commissionsTable_Numerator1e6 !! (monthsRemaining + 1)
    ------------------
    {-- 
    The calculation is indeed rounding down (truncating) the result.
    This approach favors the protocol over the users.
    Users will always receive either the exact calculated amount of tokens or slightly less, never more.
    Any fractional parts are effectively kept by the protocol.
    Over many transactions, this could accumulate to a non-trivial amount in favor of the protocol.
    --}
    ------------------
    !userFT = TxRatio.truncate (TxRatio.unsafeRatio (commissionsAcumulated_Numerator1e6 *  deposit) 1_000_000)
    ------------------
    !commissionsFT = deposit - userFT
    !commissionsRatePerMonth
        | monthsRemaining == 0 = TxRatio.fromInteger 0
        | otherwise =  TxRatio.unsafeRatio commissionsFT monthsRemaining
    !commissions_FT_Rate1e6_PerMonth = OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator commissionsRatePerMonth

--------------------------------------------------------------------------------2

{-# INLINEABLE calculateWithdrawCommissionsUsingMonths #-}
calculateWithdrawCommissionsUsingMonths :: [Integer] -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> Integer ->  (Integer, Integer, Integer)
calculateWithdrawCommissionsUsingMonths commissionsTable_Numerator1e6 deadline date withdraw investUnit_Granularity = (commissionsForUserFTToGetBack, withdrawPlusCommissionsGetBack, commissions_FT_Rate1e6_PerMonth)
    where
    -- las comisiones son por en realidad por año y en basic points BP multiplicados por 1e3 o lo que es igual 10e2 = 1_000
    -- la formula de comisiones es = (1 - comisiones por periodo en porcentaje del 0 al 1) ^ (periodos restantes + 1)
    -- el periodo aqui lo voy a calcular en meses
    -- eso significa que al valor de commissionPerYearInBPx1e3 tengo que dividirlo por
    -- 10e2 para pasarlo a bp
    -- 100 para pasarlo a porcentaje normal del 1 al 100
    -- 100 para pasarlo a porcentaje del 0 al 1
    -- 12 para pasarlo a meses
    -- defino den = 1e3 * 100 * 100 * 12 = 1000 * 100 * 100 * 12 = 120_000_000
    -- commissionesPerMonthPct0to1 = TxRatio.unsafeRatio commissionPerYearInBPx1e3 den
    -- pero en lugar de calcular el rational y leugo volver a separarlo en num y den para llamar a la potencia
    -- voy a calcular primero el 1 - commissionesPerMonthPct0to1 de esta forma:
    -- commissionesToUse = TxRatio.unsafeRatio (den - commissionPerYearInBPx1e3) den
    -- pero este Rational tampoco hace falta crearlo, puedo usar directamente esos num y den para llamar a la potencia
    -- commissionsAcumulated = OnChainHelpers.powRational (den - commissionPerYearInBPx1e3) den (daysRemaining + 1)
    ------------------
    !monthsRemaining = getRemainingMonths deadline date
    ------------------
    -- !den = 120_000_000
    -- !commissionsAcumulatedNotIncludingThisPeriod = OnChainHelpers.powRational (den - commissionPerYearInBPx1e3) den monthsRemaining
    -- !userFT'forCalculationsOfCommissionsToGetBack = TxRatio.truncate (commissionsAcumulatedNotIncludingThisPeriod * TxRatio.fromInteger withdraw)
    ------------------
    -- Estoy usando una tabla de comisiones pre calculada
    -- la tabla tiene en cada posicion el valor de las comisiones acumuladas para ese cantidad de mes restantes
    -- ademas esta guardada como enteros x 1e6, por lo que para obtener el valor real hay que dividir por 1e6
    -- aprovechando que es entero, primero calculo la multiplicacion por withdraw, que es entero, y luego lo divido por 1e6
    ------------------
    !commissionsAcumulatedNotIncludingThisPeriod_Numerator1e6 = commissionsTable_Numerator1e6 !! monthsRemaining
    !userFT'forCalculationsOfCommissionsToGetBack = TxRatio.truncate (TxRatio.unsafeRatio (commissionsAcumulatedNotIncludingThisPeriod_Numerator1e6 *  withdraw) 1_000_000)
    ------------------
    -- Ajustar para que sea múltiplo de la granularidad
    !userFT'forCalculationsOfCommissionsToGetBackAdjusted = (userFT'forCalculationsOfCommissionsToGetBack `divide` investUnit_Granularity) * investUnit_Granularity
    -- Recalcular commissionsForUserFTToGetBack basado en el valor ajustado
    !commissionsForUserFTToGetBack = withdraw - userFT'forCalculationsOfCommissionsToGetBackAdjusted
    !withdrawPlusCommissionsGetBack = withdraw + commissionsForUserFTToGetBack
    !commissionsRatePerMonth
        | monthsRemaining == 0 = TxRatio.fromInteger 0
        | otherwise =  TxRatio.unsafeRatio commissionsForUserFTToGetBack monthsRemaining
    !commissions_FT_Rate1e6_PerMonth = OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator commissionsRatePerMonth

{-# INLINEABLE calculateWithdrawCommissionsAvailable #-}
calculateWithdrawCommissionsAvailable :: [Integer] -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> Integer ->  Integer
calculateWithdrawCommissionsAvailable commissionsTable_Numerator1e6 deadline date withdraw investUnit_Granularity = commissionsForUserFTToGetBack
    where
    !monthsRemaining = getRemainingMonths deadline date
    ------------------
    !commissionsAcumulatedNotIncludingThisPeriod_Numerator1e6 = commissionsTable_Numerator1e6 !! monthsRemaining
    !userFT'forCalculationsOfCommissionsToGetBack = TxRatio.truncate (TxRatio.unsafeRatio (commissionsAcumulatedNotIncludingThisPeriod_Numerator1e6 *  withdraw) 1_000_000)
    ------------------
    -- Ajustar para que sea múltiplo de la granularidad
    !userFT'forCalculationsOfCommissionsToGetBackAdjusted = (userFT'forCalculationsOfCommissionsToGetBack `divide` investUnit_Granularity) * investUnit_Granularity
    -- Recalcular commissionsForUserFTToGetBack basado en el valor ajustado
    !commissionsForUserFTToGetBack = withdraw - userFT'forCalculationsOfCommissionsToGetBackAdjusted

{-# INLINEABLE calculateWithdrawCommissionsRate  #-}
calculateWithdrawCommissionsRate :: LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer ->  Integer
calculateWithdrawCommissionsRate deadline date commissionsForUserFTToGetBack = commissions_FT_Rate1e6_PerMonth
    where
    !monthsRemaining = getRemainingMonths deadline date
    ------------------
    !commissionsRatePerMonth
        | monthsRemaining == 0 = TxRatio.fromInteger 0
        | otherwise =  TxRatio.unsafeRatio commissionsForUserFTToGetBack monthsRemaining
    !commissions_FT_Rate1e6_PerMonth = OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator commissionsRatePerMonth

--------------------------------------------------------------------------------2

{-# INLINEABLE getCommissionsAvailable #-}
getCommissionsAvailable :: LedgerApiV2.POSIXTime ->FundHoldingT.FundHoldingDatumType ->  Integer -> Integer -> LedgerApiV2.POSIXTime -> Integer
getCommissionsAvailable deadline fundHoldingDatum_In shareBPx1e2 taken date =
    let
        !monthsRemainingRational = getRemainingMonths deadline date
        !totalCommisions = FundHoldingT.hdSubtotal_FT_Commissions fundHoldingDatum_In
        !rate = TxRatio.unsafeRatio (FundHoldingT.hdSubtotal_FT_Commissions_Rate1e6_PerMonth fundHoldingDatum_In) 1_000_000
        !commisionsReady = TxRatio.fromInteger totalCommisions - (TxRatio.fromInteger monthsRemainingRational * rate)
        -- shareBPx1e2 = shareBP * 100
        -- 1BP to decimal 1/10_000
        -- 1BPx1e2 to decimal 1/(10_000 * 100) = 1/1_000_000
        !shareDecimal = TxRatio.unsafeRatio shareBPx1e2 1_000_000
        !commisionsReady_share = commisionsReady * shareDecimal
    in TxRatio.truncate commisionsReady_share - taken

--------------------------------------------------------------------------------2
