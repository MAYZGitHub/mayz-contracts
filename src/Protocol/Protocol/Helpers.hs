{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

module Protocol.Protocol.Helpers where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Ledger.Address          as LedgerAddress
import qualified Plutus.V2.Ledger.Api    as LedgerApiV2
import           PlutusTx.Prelude

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.Types           as T
import qualified Protocol.Protocol.Types as T
import qualified Ledger.Value as LedgerValue

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

    
{-# INLINEABLE mkUpdated_Protocol_Datum_With_NormalChanges #-}
mkUpdated_Protocol_Datum_With_NormalChanges :: T.ProtocolDatumType -> LedgerAddress.PaymentPubKey -> LedgerApiV2.POSIXTime -> [T.WalletPaymentPKH] -> [T.WalletPaymentPKH] -> LedgerApiV2.CurrencySymbol -> [T.FundCategory] -> T.MinMaxDef LedgerApiV2.POSIXTime -> LedgerValue.AssetClass -> Integer -> Integer -> T.MinMaxDef Integer -> T.MinMaxDef Integer -> T.MinMaxDef Integer -> Integer -> Integer -> Integer -> Integer ->  T.ProtocolDatumType
mkUpdated_Protocol_Datum_With_NormalChanges !protocolDatum_In !oraclePaymentPubKey !oracleData_Valid_Time !admins !delegatorsAdmins !tokenAdminPolicy_CS !fundCategories !fundLifeTime !tokenMAYZ_AC !requiredMAYZForSwapOffer !requiredMAYZForBuyOrder !commissionFund_PerYear_InBPx1e3 !commissionSwapOffer_InBPx1e3 !commissionBuyOrder_InBPx1e3 !share_InBPx1e2_Protocol !share_InBPx1e2_Managers !share_InBPx1e2_Delegators !maxDepositAndWithdraw =
    protocolDatum_In {
        T.pdOraclePaymentPubKey = oraclePaymentPubKey,
        T.pdOracleData_Valid_Time = oracleData_Valid_Time,
        T.pdAdmins = admins,
        T.pdDelegatorsAdmins = delegatorsAdmins,
        T.pdTokenAdminPolicy_CS = tokenAdminPolicy_CS,
        T.pdFundCategories = fundCategories,
        T.pdFundLifeTime = fundLifeTime,
        T.pdTokenMAYZ_AC = tokenMAYZ_AC,
        T.pdRequiredMAYZForSwapOffer = requiredMAYZForSwapOffer,
        T.pdRequiredMAYZForBuyOrder = requiredMAYZForBuyOrder,
        T.pdCommissionFund_PerYear_InBPx1e3 = commissionFund_PerYear_InBPx1e3,
        T.pdCommissionSwapOffer_InBPx1e3 = commissionSwapOffer_InBPx1e3,
        T.pdCommissionBuyOrder_InBPx1e3 = commissionBuyOrder_InBPx1e3,
        T.pdShare_InBPx1e2_Protocol = share_InBPx1e2_Protocol,
        T.pdShare_InBPx1e2_Managers = share_InBPx1e2_Managers,
        T.pdShare_InBPx1e2_Delegators = share_InBPx1e2_Delegators,
        T.pdMaxDepositAndWithdraw = maxDepositAndWithdraw
    }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_Protocol_Datum_With_MinADAChanged #-}
mkUpdated_Protocol_Datum_With_MinADAChanged :: T.ProtocolDatumType -> Integer -> T.ProtocolDatumType
mkUpdated_Protocol_Datum_With_MinADAChanged !protocolDatum_In !newMinADA =
    protocolDatum_In {
        T.pdMinADA = newMinADA
    }

--------------------------------------------------------------------------------2
