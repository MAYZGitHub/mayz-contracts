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

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_Protocol_Datum_With_NormalChanges #-}
mkUpdated_Protocol_Datum_With_NormalChanges :: T.ProtocolDatumType -> LedgerAddress.PaymentPubKey -> [T.WalletPaymentPKH] -> LedgerApiV2.CurrencySymbol -> [T.FundCategory] -> T.MinMaxDef LedgerApiV2.POSIXTime -> Integer -> Integer -> T.MinMaxDef Integer -> T.MinMaxDef Integer -> T.MinMaxDef Integer -> Integer -> Integer -> Integer -> [T.WalletPaymentPKH] -> T.ProtocolDatumType
mkUpdated_Protocol_Datum_With_NormalChanges !protocolDatum_In !oraclePaymentPubKey !admins !tokenAdminPolicy_CS !fundCategories !fundLifeTime !requiredMAYZForSellOffer !requiredMAYZForBuyOrder !commissionFund_PerYear_InBPx1e3 !commissionSellOffer_InBPx1e3 !commissionBuyOrder_InBPx1e3 !share_InBPx1e2_Protocol !share_InBPx1e2_Delegators !share_InBPx1e2_Managers !delegatorsAdmins =
    protocolDatum_In {
        T.pdOraclePaymentPubKey = oraclePaymentPubKey,
        T.pdAdmins = admins,
        T.pdTokenAdminPolicy_CS = tokenAdminPolicy_CS,
        T.pdFundCategories = fundCategories,
        T.pdFundLifeTime = fundLifeTime,
        T.pdRequiredMAYZForSellOffer = requiredMAYZForSellOffer,
        T.pdRequiredMAYZForBuyOrder = requiredMAYZForBuyOrder,
        T.pdCommissionFund_PerYear_InBPx1e3 = commissionFund_PerYear_InBPx1e3,
        T.pdCommissionSellOffer_InBPx1e3 = commissionSellOffer_InBPx1e3,
        T.pdCommissionBuyOrder_InBPx1e3 = commissionBuyOrder_InBPx1e3,
        T.pdShare_InBPx1e2_Protocol = share_InBPx1e2_Protocol,
        T.pdShare_InBPx1e2_Delegators = share_InBPx1e2_Delegators,
        T.pdShare_InBPx1e2_Managers = share_InBPx1e2_Managers,
        T.pdDelegatorsAdmins = delegatorsAdmins
    }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_Protocol_Datum_With_MinADAChanged #-}
mkUpdated_Protocol_Datum_With_MinADAChanged :: T.ProtocolDatumType -> Integer -> T.ProtocolDatumType
mkUpdated_Protocol_Datum_With_MinADAChanged !protocolDatum_In !newMinADA =
    protocolDatum_In {
        T.pdMinADA = newMinADA
    }

--------------------------------------------------------------------------------2
