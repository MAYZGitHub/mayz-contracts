{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

module Protocol.Protocol.Types where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Data.Aeson           as DataAeson (FromJSON, ToJSON)
import qualified Data.OpenApi.Schema  as DataOpenApiSchema (ToSchema)
import qualified GHC.Generics         as GHCGenerics (Generic)
import qualified Ledger.Address       as LedgerAddress
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude              as P
import qualified Schema

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.Types        as T
import qualified Protocol.Constants   as T
import qualified Protocol.Types       as T
import qualified Generic.OnChainHelpers as OnChainHelpers

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

--------------------------------------------------------------------------------2
-- Params
--------------------------------------------------------------------------------2

newtype PolicyParams
    = PolicyParams { ppProtocolPolicyID_TxOutRef :: LedgerApiV2.TxOutRef }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq PolicyParams where
    {-# INLINABLE (==) #-}
    pp1 == pp2 =
        ppProtocolPolicyID_TxOutRef pp1 == ppProtocolPolicyID_TxOutRef pp2

PlutusTx.makeLift ''PolicyParams

PlutusTx.makeIsDataIndexed ''PolicyParams [('PolicyParams, 0)]

data ValidatorParams
    = ValidatorParams
          { vpProtocolPolicyID_CS          :: T.CS
          , vpTokenEmergencyAdminPolicy_CS :: LedgerApiV2.CurrencySymbol
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

-- instance Schema.ToSchema ValidatorParams where
--     toSchema = Schema.FormSchemaUnit

instance Eq ValidatorParams where
    {-# INLINABLE (==) #-}
    pp1 == pp2 =
        vpProtocolPolicyID_CS pp1 == vpProtocolPolicyID_CS pp2
        && vpTokenEmergencyAdminPolicy_CS pp1 == vpTokenEmergencyAdminPolicy_CS pp2

PlutusTx.makeLift ''ValidatorParams

PlutusTx.makeIsDataIndexed ''ValidatorParams [('ValidatorParams, 0)]

--------------------------------------------------------------------------------2
-- Datums
--------------------------------------------------------------------------------2
data FundCategory
    = FundCategory
          { fcCategoryNumber :: Integer
          , fcRequiredMAYZ   :: Integer
          , fcMaxUI          :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq FundCategory where
    {-# INLINABLE (==) #-}
    mi1 == mi2 =
        fcCategoryNumber mi1 == fcCategoryNumber mi2
            && fcRequiredMAYZ mi1 == fcRequiredMAYZ mi2
            && fcMaxUI mi1 == fcMaxUI mi2

instance Ord FundCategory where
    {-# INLINABLE compare #-}
    compare :: FundCategory -> FundCategory -> Ordering
    compare fc1 fc2
        | fcCategoryNumber fc1 < fcCategoryNumber fc2 = LT
        | fcCategoryNumber fc1 == fcCategoryNumber fc2 && fcRequiredMAYZ fc1 < fcRequiredMAYZ fc2 = LT
        | fcCategoryNumber fc1 == fcCategoryNumber fc2
                && fcRequiredMAYZ fc1 == fcRequiredMAYZ fc2
                && fcMaxUI fc1 < fcMaxUI fc2 = LT
        | otherwise = GT

PlutusTx.makeIsDataIndexed ''FundCategory [('FundCategory, 0)]

--------------------------------------------------------------------------------2
data MinMaxDef a
    = MinMaxDef
          { mmdMin :: a
          , mmdMax :: a
          , mmdDef :: a
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance (Eq a) => Eq (MinMaxDef a) where
    {-# INLINABLE (==) #-}
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

{-# INLINABLE mkMinMaxDef #-}
mkMinMaxDef :: a -> a -> a -> MinMaxDef a
mkMinMaxDef min' max' def' =
    MinMaxDef {mmdMin = min', mmdMax = max', mmdDef = def'}

--------------------------------------------------------------------------------2
data ProtocolDatumType
    = ProtocolDatumType
          { pdProtocolFactoryVersion          :: Integer
          , pdScriptPolicyID_CS               :: T.CS
          , pdScriptValidator_Hash            :: LedgerApiV2.ValidatorHash
          , pdOraclePaymentPubKey             :: LedgerAddress.PaymentPubKey
          , pdAdmins                          :: [T.WalletPaymentPKH]
          , pdDelegatorsAdmins                     :: [T.WalletPaymentPKH]
          , pdTokenAdminPolicy_CS             :: LedgerApiV2.CurrencySymbol
          , pdFundCategories                  :: [FundCategory]
          , pdFundLifeTime                    :: MinMaxDef LedgerApiV2.POSIXTime
          , pdRequiredMAYZForSellOffer        :: Integer
          , pdRequiredMAYZForBuyOrder         :: Integer
          , pdCommissionFund_PerYear_InBPx1e3 :: MinMaxDef Integer
          , pdCommissionSellOffer_InBPx1e3    :: MinMaxDef Integer
          , pdCommissionBuyOrder_InBPx1e3     :: MinMaxDef Integer
          , pdShare_InBPx1e2_Protocol          :: Integer
          , pdShare_InBPx1e2_Delegators              :: Integer
          , pdShare_InBPx1e2_Managers            :: Integer
          , pdMinADA                          :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq ProtocolDatumType where
    {-# INLINABLE (==) #-}
    ps1 == ps2 =
        pdProtocolFactoryVersion ps1 == pdProtocolFactoryVersion ps2
            && pdScriptPolicyID_CS ps1 == pdScriptPolicyID_CS ps2
            && pdScriptValidator_Hash ps1 == pdScriptValidator_Hash ps2
            && pdOraclePaymentPubKey ps1 == pdOraclePaymentPubKey ps2
            && pdAdmins ps1 == pdAdmins ps2
            && pdTokenAdminPolicy_CS ps1 == pdTokenAdminPolicy_CS ps2
            && pdFundCategories ps1 == pdFundCategories ps2
            && pdFundLifeTime ps1 == pdFundLifeTime ps2
            && pdRequiredMAYZForSellOffer ps1 == pdRequiredMAYZForSellOffer ps2
            && pdRequiredMAYZForBuyOrder ps1 == pdRequiredMAYZForBuyOrder ps2
            && pdCommissionFund_PerYear_InBPx1e3 ps1 == pdCommissionFund_PerYear_InBPx1e3 ps2
            && pdCommissionSellOffer_InBPx1e3 ps1 == pdCommissionSellOffer_InBPx1e3 ps2
            && pdCommissionBuyOrder_InBPx1e3 ps1 == pdCommissionBuyOrder_InBPx1e3 ps2
            && pdShare_InBPx1e2_Protocol ps1 == pdShare_InBPx1e2_Protocol ps2
            && pdShare_InBPx1e2_Delegators ps1 == pdShare_InBPx1e2_Delegators ps2
            && pdShare_InBPx1e2_Managers ps1 == pdShare_InBPx1e2_Managers ps2
            && pdDelegatorsAdmins ps1 == pdDelegatorsAdmins ps2
            && pdMinADA ps1 == pdMinADA ps2

instance T.HasAdmins ProtocolDatumType where
    {-# INLINABLE getAdmins #-}
    getAdmins = pdAdmins

instance T.HasAdminToken ProtocolDatumType where
    {-# INLINABLE getAdminToken_CS #-}
    getAdminToken_CS = pdTokenAdminPolicy_CS

PlutusTx.makeIsDataIndexed ''ProtocolDatumType [('ProtocolDatumType, 0)]

newtype ValidatorDatum
    = ProtocolDatum ProtocolDatumType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq ValidatorDatum where
    {-# INLINABLE (==) #-}
    ProtocolDatum mps1 == ProtocolDatum mps2 = mps1 == mps2

PlutusTx.makeIsDataIndexed ''ValidatorDatum [('ProtocolDatum, 0)]

{-# INLINABLE getProtocol_DatumType #-}
getProtocol_DatumType :: ValidatorDatum -> ProtocolDatumType
getProtocol_DatumType (ProtocolDatum sdType) = sdType

{-# INLINEABLE getProtocol_DatumType_From_UTxO #-}
getProtocol_DatumType_From_UTxO :: LedgerApiV2.TxOut -> ProtocolDatumType
getProtocol_DatumType_From_UTxO utxo = case OnChainHelpers.getInlineDatum_From_TxOut @ValidatorDatum utxo of
                    Nothing     -> P.error "No Protocol Datum found"
                    Just datum' -> getProtocol_DatumType datum'

instance T.ShowDatum ValidatorDatum where
    showCborAsDatumType cbor =
        case LedgerApiV2.fromBuiltinData @ValidatorDatum cbor of
            Nothing -> Nothing
            Just d  -> Just $ P.show d

--------------------------------------------------------------------------------2
{-# INLINABLE mkProtocolDatumType #-}
mkProtocolDatumType ::
         T.CS
    -> LedgerApiV2.ValidatorHash
    -> LedgerAddress.PaymentPubKey
    -> [T.WalletPaymentPKH]
    -> LedgerApiV2.CurrencySymbol
    -> [FundCategory]
    -> MinMaxDef LedgerApiV2.POSIXTime
    -> Integer
    -> Integer
    -> MinMaxDef Integer
    -> MinMaxDef Integer
    -> MinMaxDef Integer
    -> Integer
    -> Integer
    -> Integer
    -> [T.WalletPaymentPKH]
    -> Integer
    -> ProtocolDatumType
mkProtocolDatumType
    scriptPolicyID_CS
    scriptValidator_Hash
    oraclePaymentPubKey
    admins
    tokenAdminPolicy_CS
    fundCategories
    fundLifeTime
    requiredMAYZForSellOffer
    requiredMAYZForBuyOrder
    commissionFund_PerYear_InBPx1e3
    commissionSellOffer_InBPx1e3
    commissionBuyOrder_InBPx1e3
    share_InBPx1e2_Protocol
    share_InBPx1e2_Delegators
    share_InBPx1e2_Managers
    delegatorsAdmins
    minADA =
        let !adminsOrdered = sort admins
            !fundCategoriesOrdered = sort fundCategories
            !delegatorsAdminsOrdered = sort delegatorsAdmins
        in ProtocolDatumType
                { pdProtocolFactoryVersion = T.protocolFactoryVersion
                , pdScriptPolicyID_CS = scriptPolicyID_CS
                , pdScriptValidator_Hash = scriptValidator_Hash
                , pdOraclePaymentPubKey = oraclePaymentPubKey
                , pdAdmins = adminsOrdered
                , pdTokenAdminPolicy_CS = tokenAdminPolicy_CS
                , pdFundCategories = fundCategoriesOrdered
                , pdFundLifeTime = fundLifeTime
                , pdRequiredMAYZForSellOffer = requiredMAYZForSellOffer
                , pdRequiredMAYZForBuyOrder = requiredMAYZForBuyOrder
                , pdCommissionFund_PerYear_InBPx1e3 = commissionFund_PerYear_InBPx1e3
                , pdCommissionSellOffer_InBPx1e3 = commissionSellOffer_InBPx1e3
                , pdCommissionBuyOrder_InBPx1e3 = commissionBuyOrder_InBPx1e3
                , pdShare_InBPx1e2_Protocol = share_InBPx1e2_Protocol
                , pdShare_InBPx1e2_Delegators = share_InBPx1e2_Delegators
                , pdShare_InBPx1e2_Managers = share_InBPx1e2_Managers
                , pdDelegatorsAdmins = delegatorsAdminsOrdered
                , pdMinADA = minADA
                }

{-# INLINABLE mkProtocolDatum #-}
mkProtocolDatum ::
         T.CS
    -> LedgerApiV2.ValidatorHash
    -> LedgerAddress.PaymentPubKey
    -> [T.WalletPaymentPKH]
    -> LedgerApiV2.CurrencySymbol
    -> [FundCategory]
    -> MinMaxDef LedgerApiV2.POSIXTime
    -> Integer
    -> Integer
    -> MinMaxDef Integer
    -> MinMaxDef Integer
    -> MinMaxDef Integer
    -> Integer
    -> Integer
    -> Integer
    -> [T.WalletPaymentPKH]
    -> Integer
    -> ValidatorDatum
mkProtocolDatum
    scriptPolicyID_CS
    scriptValidator_Hash
    oraclePaymentPubKey
    admins
    tokenAdminPolicy_CS
    fundCategories
    fundLifeTime
    requiredMAYZForSellOffer
    requiredMAYZForBuyOrder
    commissionFund_PerYear_InBPx1e3
    commissionSellOffer_InBPx1e3
    commissionBuyOrder_InBPx1e3
    share_InBPx1e2_Protocol
    share_InBPx1e2_Delegators
    share_InBPx1e2_Managers
    delegatorsAdmins
    minADA =
        ProtocolDatum
            $ mkProtocolDatumType
                    scriptPolicyID_CS
                    scriptValidator_Hash
                    oraclePaymentPubKey
                    admins
                    tokenAdminPolicy_CS
                    fundCategories
                    fundLifeTime
                    requiredMAYZForSellOffer
                    requiredMAYZForBuyOrder
                    commissionFund_PerYear_InBPx1e3
                    commissionSellOffer_InBPx1e3
                    commissionBuyOrder_InBPx1e3
                    share_InBPx1e2_Protocol
                    share_InBPx1e2_Delegators
                    share_InBPx1e2_Managers
                    delegatorsAdmins
                    minADA

mkDatum :: ProtocolDatumType -> LedgerApiV2.Datum
mkDatum = LedgerApiV2.Datum . LedgerApiV2.toBuiltinData . ProtocolDatum


--------------------------------------------------------------------------------2
-- ValidatorRedeemer
--------------------------------------------------------------------------------2

data ValidatorRedeemerDatumUpdateType = ValidatorRedeemerDatumUpdateType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerDatumUpdateType where
    {-# INLINABLE (==) #-}
    r1 == r2 =  r1 ==  r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerDatumUpdateType
    [('ValidatorRedeemerDatumUpdateType, 0)]

--------------------------------------------------------------------------------2

data ValidatorRedeemerUpdateMinADAType = ValidatorRedeemerUpdateMinADAType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerUpdateMinADAType where
    {-# INLINABLE (==) #-}
    r1 == r2 =  r1 ==  r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerUpdateMinADAType
    [('ValidatorRedeemerUpdateMinADAType, 0)]

--------------------------------------------------------------------------------2

data ValidatorRedeemerEmergencyType = ValidatorRedeemerEmergencyType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerEmergencyType where
    {-# INLINABLE (==) #-}
    r1 == r2 =  r1 ==  r2

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
    {-# INLINABLE (==) #-}
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
