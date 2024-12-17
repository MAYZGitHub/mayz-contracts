{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

module Protocol.Protocol.Types where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Data.Aeson as DataAeson (FromJSON, ToJSON)
import qualified Data.OpenApi.Schema as DataOpenApiSchema (ToSchema)
import qualified GHC.Generics as GHCGenerics (Generic)
import qualified Ledger.Address as LedgerAddress
import qualified Ledger.Value as LedgerValue
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Schema
import qualified Prelude as P

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.OnChainHelpers as OnChainHelpers
import qualified Generic.Types as T
import qualified Protocol.Constants as T
import qualified Protocol.Types as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

-- Any change in the logic, datum or redeemer must change the version of the protocolVersion
protocolVersion :: Integer
protocolVersion = 4

ownVersion :: Integer
ownVersion = T.mkVersionWithDependency [] protocolVersion

--------------------------------------------------------------------------------2
-- Params
--------------------------------------------------------------------------------2

newtype PolicyParams = PolicyParams {ppProtocolPolicyID_TxOutRef :: LedgerApiV2.TxOutRef}
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq PolicyParams where
    {-# INLINEABLE (==) #-}
    pp1 == pp2 =
        ppProtocolPolicyID_TxOutRef pp1 == ppProtocolPolicyID_TxOutRef pp2

PlutusTx.makeLift ''PolicyParams

PlutusTx.makeIsDataIndexed ''PolicyParams [('PolicyParams, 0)]

data ValidatorParams = ValidatorParams
    { vpProtocolPolicyID_CS :: T.CS
    , vpTokenEmergencyAdminPolicy_CS :: LedgerApiV2.CurrencySymbol
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

-- instance Schema.ToSchema ValidatorParams where
--     toSchema = Schema.FormSchemaUnit

instance Eq ValidatorParams where
    {-# INLINEABLE (==) #-}
    pp1 == pp2 =
        vpProtocolPolicyID_CS pp1 == vpProtocolPolicyID_CS pp2
            && vpTokenEmergencyAdminPolicy_CS pp1 == vpTokenEmergencyAdminPolicy_CS pp2

PlutusTx.makeLift ''ValidatorParams

PlutusTx.makeIsDataIndexed ''ValidatorParams [('ValidatorParams, 0)]

--------------------------------------------------------------------------------2
-- Datums
--------------------------------------------------------------------------------2
data FundCategory = FundCategory
    { fcCategoryNumber :: Integer -- Sequential ID starting at 0
    , fcRequiredMAYZ :: Integer -- MAYZ tokens needed for fund
    , fcMaxUI :: Integer -- Max Investment Unit value
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq FundCategory where
    {-# INLINEABLE (==) #-}
    mi1 == mi2 =
        fcCategoryNumber mi1 == fcCategoryNumber mi2
            && fcRequiredMAYZ mi1 == fcRequiredMAYZ mi2
            && fcMaxUI mi1 == fcMaxUI mi2

instance Ord FundCategory where
    {-# INLINEABLE compare #-}
    compare :: FundCategory -> FundCategory -> Ordering
    compare fc1 fc2
        | fcCategoryNumber fc1 < fcCategoryNumber fc2 = LT
        | fcCategoryNumber fc1 == fcCategoryNumber fc2 && fcRequiredMAYZ fc1 < fcRequiredMAYZ fc2 = LT
        | fcCategoryNumber fc1 == fcCategoryNumber fc2
            && fcRequiredMAYZ fc1 == fcRequiredMAYZ fc2
            && fcMaxUI fc1 < fcMaxUI fc2 =
            LT
        | otherwise = GT

PlutusTx.makeIsDataIndexed ''FundCategory [('FundCategory, 0)]

--------------------------------------------------------------------------------2
data MinMaxDef a = MinMaxDef
    { mmdMin :: a
    , mmdMax :: a
    , mmdDef :: a
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance (Eq a) => Eq (MinMaxDef a) where
    {-# INLINEABLE (==) #-}
    mi1 == mi2 =
        mmdMin mi1 == mmdMin mi2
            && mmdMax mi1 == mmdMax mi2
            && mmdDef mi1 == mmdDef mi2

-- Method to check the range
isInRange :: (Ord a) => MinMaxDef a -> a -> Bool
isInRange mmd value = mmdMin mmd <= value && value <= mmdMax mmd

isValidMinMaxDef :: (Ord a) => MinMaxDef a -> Bool
isValidMinMaxDef mmd = mmdMin mmd <= mmdMax mmd && mmdMin mmd <= mmdDef mmd && mmdDef mmd <= mmdMax mmd

instance DataOpenApiSchema.ToSchema (MinMaxDef Integer)

instance DataOpenApiSchema.ToSchema (MinMaxDef LedgerApiV2.POSIXTime)

PlutusTx.makeIsDataIndexed ''MinMaxDef [('MinMaxDef, 0)]

{-# INLINEABLE mkMinMaxDef #-}
mkMinMaxDef :: a -> a -> a -> MinMaxDef a
mkMinMaxDef min' max' def' =
    MinMaxDef {mmdMin = min', mmdMax = max', mmdDef = def'}

--------------------------------------------------------------------------------2
data ProtocolDatumType = ProtocolDatumType
    { -- Version Control
      pdProtocolVersion :: Integer -- Current version number
    , -- Script Management
      pdScriptPolicyID_CS :: T.CS -- Script policy ID
    , pdScriptValidator_Hash :: LedgerApiV2.ValidatorHash -- Script validator hash
    , -- Oracle Configuration
      pdOraclePaymentPubKey :: LedgerAddress.PaymentPubKey -- Oracle's verification key
    , pdOracleData_Valid_Time :: LedgerApiV2.POSIXTime -- Time window for oracle data validity
    , -- Access Control
      pdAdmins :: [T.WalletPaymentPKH] -- Protocol admin PKHs
    , pdDelegatorsAdmins :: [T.WalletPaymentPKH] -- Delegator admin PKHs
    , pdTokenAdminPolicy_CS :: T.CS -- Admin token policy ID
    , -- Fund Configuration
      pdFundCategories :: [FundCategory] -- Available fund categories
    , pdFundLifeTime :: MinMaxDef LedgerApiV2.POSIXTime -- Fund duration constraints
    , -- Required Stakes
      pdTokenMAYZ_AC :: LedgerValue.AssetClass -- MAYZ token identifier
    , pdRequiredMAYZForSwapOffer :: Integer -- MAYZ needed for swap offers
    , pdRequiredMAYZForBuyOrder :: Integer -- MAYZ needed for buy orders
    , -- Commission Configuration (basis points x 1000)
      pdCommissionFund_PerYear_InBPx1e3 :: MinMaxDef Integer -- Fund commission range
    , pdCommissionSwapOffer_InBPx1e3 :: MinMaxDef Integer -- Swap offer commission range
    , pdCommissionBuyOrder_InBPx1e3 :: MinMaxDef Integer -- Buy order commission range
    , -- Commission Distribution (basis points x 100)
      -- Must sum to 1,000,000 = 100%
      pdShare_InBPx1e2_Protocol :: Integer -- Protocol's share
    , pdShare_InBPx1e2_Managers :: Integer -- Fund managers' share
    , pdShare_InBPx1e2_Delegators :: Integer -- Delegators' share
    , -- Operation Limits
      pdMaxDepositAndWithdraw :: Integer -- Maximum single transaction amount
    , -- Minimum ADA
      pdMinADA :: Integer -- Minimum ADA used in UTXO
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq ProtocolDatumType where
    {-# INLINEABLE (==) #-}
    ps1 == ps2 =
        pdProtocolVersion ps1 == pdProtocolVersion ps2
            && pdScriptPolicyID_CS ps1 == pdScriptPolicyID_CS ps2
            && pdScriptValidator_Hash ps1 == pdScriptValidator_Hash ps2
            && pdOraclePaymentPubKey ps1 == pdOraclePaymentPubKey ps2
            && pdAdmins ps1 == pdAdmins ps2
            && pdTokenAdminPolicy_CS ps1 == pdTokenAdminPolicy_CS ps2
            && pdFundCategories ps1 == pdFundCategories ps2
            && pdFundLifeTime ps1 == pdFundLifeTime ps2
            && pdTokenMAYZ_AC ps1 == pdTokenMAYZ_AC ps2
            && pdRequiredMAYZForSwapOffer ps1 == pdRequiredMAYZForSwapOffer ps2
            && pdRequiredMAYZForBuyOrder ps1 == pdRequiredMAYZForBuyOrder ps2
            && pdCommissionFund_PerYear_InBPx1e3 ps1 == pdCommissionFund_PerYear_InBPx1e3 ps2
            && pdCommissionSwapOffer_InBPx1e3 ps1 == pdCommissionSwapOffer_InBPx1e3 ps2
            && pdCommissionBuyOrder_InBPx1e3 ps1 == pdCommissionBuyOrder_InBPx1e3 ps2
            && pdShare_InBPx1e2_Protocol ps1 == pdShare_InBPx1e2_Protocol ps2
            && pdShare_InBPx1e2_Managers ps1 == pdShare_InBPx1e2_Managers ps2
            && pdShare_InBPx1e2_Delegators ps1 == pdShare_InBPx1e2_Delegators ps2
            && pdDelegatorsAdmins ps1 == pdDelegatorsAdmins ps2
            && pdOracleData_Valid_Time ps1 == pdOracleData_Valid_Time ps2
            && pdMaxDepositAndWithdraw ps1 == pdMaxDepositAndWithdraw ps2
            && pdMinADA ps1 == pdMinADA ps2

instance T.HasAdmins ProtocolDatumType where
    {-# INLINEABLE getAdmins #-}
    getAdmins = pdAdmins

instance T.HasAdminToken ProtocolDatumType where
    {-# INLINEABLE getAdminToken_CS #-}
    getAdminToken_CS = pdTokenAdminPolicy_CS

PlutusTx.makeIsDataIndexed ''ProtocolDatumType [('ProtocolDatumType, 0)]

newtype ValidatorDatum
    = ProtocolDatum ProtocolDatumType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq ValidatorDatum where
    {-# INLINEABLE (==) #-}
    ProtocolDatum mps1 == ProtocolDatum mps2 = mps1 == mps2

PlutusTx.makeIsDataIndexed ''ValidatorDatum [('ProtocolDatum, 0)]

{-# INLINEABLE getProtocol_DatumType #-}
getProtocol_DatumType :: ValidatorDatum -> ProtocolDatumType
getProtocol_DatumType (ProtocolDatum sdType) = sdType

{-# INLINEABLE getProtocol_DatumType_From_UTxO #-}
getProtocol_DatumType_From_UTxO :: LedgerApiV2.TxOut -> ProtocolDatumType
getProtocol_DatumType_From_UTxO utxo = case OnChainHelpers.getInlineDatum_From_TxOut @ValidatorDatum utxo of
    Nothing -> P.error "No Protocol Datum found"
    Just datum' -> getProtocol_DatumType datum'

instance T.ShowDatum ValidatorDatum where
    showCborAsDatumType cbor =
        case LedgerApiV2.fromBuiltinData @ValidatorDatum cbor of
            Nothing -> Nothing
            Just d -> Just $ P.show d

--------------------------------------------------------------------------------2
{-# INLINEABLE mkProtocol_DatumType #-}
mkProtocol_DatumType ::
    T.CS ->
    LedgerApiV2.ValidatorHash ->
    LedgerAddress.PaymentPubKey ->
    LedgerApiV2.POSIXTime ->
    [T.WalletPaymentPKH] ->
    [T.WalletPaymentPKH] ->
    LedgerApiV2.CurrencySymbol ->
    [FundCategory] ->
    MinMaxDef LedgerApiV2.POSIXTime ->
    LedgerValue.AssetClass ->
    Integer ->
    Integer ->
    MinMaxDef Integer ->
    MinMaxDef Integer ->
    MinMaxDef Integer ->
    Integer ->
    Integer ->
    Integer ->
    Integer ->
    Integer ->
    ProtocolDatumType
mkProtocol_DatumType
    scriptPolicyID_CS
    scriptValidator_Hash
    oraclePaymentPubKey
    oracleData_Valid_Time
    admins
    delegatorsAdmins
    tokenAdminPolicy_CS
    fundCategories
    fundLifeTime
    tokenMAYZ_AC
    requiredMAYZForSwapOffer
    requiredMAYZForBuyOrder
    commissionFund_PerYear_InBPx1e3
    commissionSwapOffer_InBPx1e3
    commissionBuyOrder_InBPx1e3
    share_InBPx1e2_Protocol
    share_InBPx1e2_Managers
    share_InBPx1e2_Delegators
    maxDepositAndWithdraw
    minADA =
        let
            !adminsOrdered = sort admins
            !fundCategoriesOrdered = sort fundCategories
            !delegatorsAdminsOrdered = sort delegatorsAdmins
        in
            ProtocolDatumType
                { pdProtocolVersion = ownVersion
                , pdScriptPolicyID_CS = scriptPolicyID_CS
                , pdScriptValidator_Hash = scriptValidator_Hash
                , pdOraclePaymentPubKey = oraclePaymentPubKey
                , pdOracleData_Valid_Time = oracleData_Valid_Time
                , pdAdmins = adminsOrdered
                , pdDelegatorsAdmins = delegatorsAdminsOrdered
                , pdTokenAdminPolicy_CS = tokenAdminPolicy_CS
                , pdFundCategories = fundCategoriesOrdered
                , pdFundLifeTime = fundLifeTime
                , pdTokenMAYZ_AC = tokenMAYZ_AC
                , pdRequiredMAYZForSwapOffer = requiredMAYZForSwapOffer
                , pdRequiredMAYZForBuyOrder = requiredMAYZForBuyOrder
                , pdCommissionFund_PerYear_InBPx1e3 = commissionFund_PerYear_InBPx1e3
                , pdCommissionSwapOffer_InBPx1e3 = commissionSwapOffer_InBPx1e3
                , pdCommissionBuyOrder_InBPx1e3 = commissionBuyOrder_InBPx1e3
                , pdShare_InBPx1e2_Protocol = share_InBPx1e2_Protocol
                , pdShare_InBPx1e2_Managers = share_InBPx1e2_Managers
                , pdShare_InBPx1e2_Delegators = share_InBPx1e2_Delegators
                , pdMaxDepositAndWithdraw = maxDepositAndWithdraw
                , pdMinADA = minADA
                }

{-# INLINEABLE mkProtocol_Datum #-}
mkProtocol_Datum ::
    T.CS ->
    LedgerApiV2.ValidatorHash ->
    LedgerAddress.PaymentPubKey ->
    LedgerApiV2.POSIXTime ->
    [T.WalletPaymentPKH] ->
    [T.WalletPaymentPKH] ->
    LedgerApiV2.CurrencySymbol ->
    [FundCategory] ->
    MinMaxDef LedgerApiV2.POSIXTime ->
    LedgerValue.AssetClass ->
    Integer ->
    Integer ->
    MinMaxDef Integer ->
    MinMaxDef Integer ->
    MinMaxDef Integer ->
    Integer ->
    Integer ->
    Integer ->
    Integer ->
    Integer ->
    ValidatorDatum
mkProtocol_Datum
    scriptPolicyID_CS
    scriptValidator_Hash
    oraclePaymentPubKey
    oracleData_Valid_Time
    admins
    delegatorsAdmins
    tokenAdminPolicy_CS
    fundCategories
    fundLifeTime
    tokenMAYZ_AC
    requiredMAYZForSwapOffer
    requiredMAYZForBuyOrder
    commissionFund_PerYear_InBPx1e3
    commissionSwapOffer_InBPx1e3
    commissionBuyOrder_InBPx1e3
    share_InBPx1e2_Protocol
    share_InBPx1e2_Managers
    share_InBPx1e2_Delegators
    maxDepositAndWithdraw
    minADA =
        ProtocolDatum $
            mkProtocol_DatumType
                scriptPolicyID_CS
                scriptValidator_Hash
                oraclePaymentPubKey
                oracleData_Valid_Time
                admins
                delegatorsAdmins
                tokenAdminPolicy_CS
                fundCategories
                fundLifeTime
                tokenMAYZ_AC
                requiredMAYZForSwapOffer
                requiredMAYZForBuyOrder
                commissionFund_PerYear_InBPx1e3
                commissionSwapOffer_InBPx1e3
                commissionBuyOrder_InBPx1e3
                share_InBPx1e2_Protocol
                share_InBPx1e2_Managers
                share_InBPx1e2_Delegators
                maxDepositAndWithdraw
                minADA

mkDatum :: ProtocolDatumType -> LedgerApiV2.Datum
mkDatum = LedgerApiV2.Datum . LedgerApiV2.toBuiltinData . ProtocolDatum

--------------------------------------------------------------------------------2
-- ValidatorRedeemer
--------------------------------------------------------------------------------2

data ValidatorRedeemerDatumUpdateType = ValidatorRedeemerDatumUpdateType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerDatumUpdateType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerDatumUpdateType
    [('ValidatorRedeemerDatumUpdateType, 0)]

--------------------------------------------------------------------------------2

data ValidatorRedeemerUpdateMinADAType = ValidatorRedeemerUpdateMinADAType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerUpdateMinADAType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerUpdateMinADAType
    [('ValidatorRedeemerUpdateMinADAType, 0)]

--------------------------------------------------------------------------------2

data ValidatorRedeemerEmergencyType = ValidatorRedeemerEmergencyType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerEmergencyType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerEmergencyType
    [('ValidatorRedeemerEmergencyType, 0)]

--------------------------------------------------------------------------------2
data ValidatorRedeemer
    = ValidatorRedeemerDatumUpdate ValidatorRedeemerDatumUpdateType
    | ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType
    | ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemer where
    {-# INLINEABLE (==) #-}
    (==) :: ValidatorRedeemer -> ValidatorRedeemer -> Bool
    ValidatorRedeemerDatumUpdate rmf1 == ValidatorRedeemerDatumUpdate rmf2 =
        rmf1 == rmf2
    ValidatorRedeemerUpdateMinADA rmf1 == ValidatorRedeemerUpdateMinADA rmf2 =
        rmf1 == rmf2
    ValidatorRedeemerEmergency rmf1 == ValidatorRedeemerEmergency rmf2 =
        rmf1 == rmf2
    _ == _ = False

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemer
    [ ('ValidatorRedeemerDatumUpdate, 0)
    , ('ValidatorRedeemerUpdateMinADA, 1)
    , ('ValidatorRedeemerEmergency, 2)
    ]

--------------------------------------------------------------------------------2

getValidatorRedeemerName :: Maybe ValidatorRedeemer -> Maybe P.String
getValidatorRedeemerName (Just (ValidatorRedeemerDatumUpdate ValidatorRedeemerDatumUpdateType)) = Just "DatumUpdate"
getValidatorRedeemerName (Just (ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType)) = Just "UpdateMinADA"
getValidatorRedeemerName (Just (ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType)) = Just "Emergency"
getValidatorRedeemerName _ = Nothing

--------------------------------------------------------------------------------2

mkMintIDRedeemer :: LedgerApiV2.Redeemer
mkMintIDRedeemer = LedgerApiV2.Redeemer $ LedgerApiV2.toBuiltinData (0 :: Integer)

mkDatumUpdateRedeemer :: LedgerApiV2.Redeemer
mkDatumUpdateRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerDatumUpdate ValidatorRedeemerDatumUpdateType

mkUpdateMinADARedeemer :: LedgerApiV2.Redeemer
mkUpdateMinADARedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType

mkEmergencyRedeemer :: LedgerApiV2.Redeemer
mkEmergencyRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType

--------------------------------------------------------------------------------2
