{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

module Protocol.BuyOrder.Types where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Data.Aeson as DataAeson
import qualified Data.OpenApi.Schema as DataOpenApiSchema
import qualified GHC.Generics as GHCGenerics
import qualified Ledger
import qualified Ledger.Value as LedgerValue
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Schema
import qualified Prelude as P

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.Types as T
import qualified Protocol.Constants as T
import qualified Protocol.Fund.Types as FundT
import qualified Protocol.Protocol.Types as ProtocolT
import qualified Protocol.Types as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

-- Any change in the logic, datum or redeemer must change the version of the buyOrderVersion
buyOrderVersion :: Integer
buyOrderVersion = 1

ownVersion :: Integer
ownVersion = T.mkVersionWithDependency [ProtocolT.protocolVersion, FundT.fundVersion] buyOrderVersion

--------------------------------------------------------------------------------2
-- Params
--------------------------------------------------------------------------------2

data PolicyParams = PolicyParams
    { ppProtocolPolicyID_CS :: T.CS
    , ppBuyOrder_Validator_Hash :: LedgerApiV2.ValidatorHash
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq PolicyParams where
    {-# INLINEABLE (==) #-}
    p1 == p2 =
        ppProtocolPolicyID_CS p1 == ppProtocolPolicyID_CS p2
            && ppBuyOrder_Validator_Hash p1 == ppBuyOrder_Validator_Hash p2

PlutusTx.makeLift ''PolicyParams
PlutusTx.makeIsDataIndexed ''PolicyParams [('PolicyParams, 0)]

newtype ValidatorParams = ValidatorParams {vpProtocolPolicyID_CS :: T.CS}
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq ValidatorParams where
    {-# INLINEABLE (==) #-}
    p1 == p2 =
        vpProtocolPolicyID_CS p1 == vpProtocolPolicyID_CS p2

PlutusTx.makeLift ''ValidatorParams
PlutusTx.makeIsDataIndexed ''ValidatorParams [('ValidatorParams, 0)]

--------------------------------------------------------------------------------2
-- Datums
--------------------------------------------------------------------------------2

data BuyOrder_DatumType = BuyOrder_DatumType
    { bodVersion :: Integer
    , bodBuyOrderPolicyID_CS :: T.CS
    , bodFundPolicy_CS :: T.CS
    , bodBuyerPaymentPKH :: T.WalletPaymentPKH
    , bodBuyerStakePKH :: Maybe T.WalletPaymentPKH
    , bodOfferedCommission_InBPx1e3 :: Integer
    , bodFT_Received :: Integer
    , bodFT_PayedAsCommission :: Integer
    , bodOrder_Status :: Integer
    , bodTokenMAYZ_AC :: LedgerValue.AssetClass
    , bodRequiredMAYZ :: Integer
    , bodMinADA :: Integer
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq BuyOrder_DatumType where
    {-# INLINEABLE (==) #-}
    sd1 == sd2 =
        bodVersion sd1 == bodVersion sd2
            && bodBuyOrderPolicyID_CS sd1 == bodBuyOrderPolicyID_CS sd2
            && bodFundPolicy_CS sd1 == bodFundPolicy_CS sd2
            && bodBuyerPaymentPKH sd1 == bodBuyerPaymentPKH sd2
            && bodBuyerStakePKH sd1 == bodBuyerStakePKH sd2
            && bodOfferedCommission_InBPx1e3 sd1 == bodOfferedCommission_InBPx1e3 sd2
            && bodFT_Received sd1 == bodFT_Received sd2
            && bodFT_PayedAsCommission sd1 == bodFT_PayedAsCommission sd2
            && bodOrder_Status sd1 == bodOrder_Status sd2
            && bodTokenMAYZ_AC sd1 == bodTokenMAYZ_AC sd2
            && bodRequiredMAYZ sd1 == bodRequiredMAYZ sd2
            && bodMinADA sd1 == bodMinADA sd2

PlutusTx.makeIsDataIndexed ''BuyOrder_DatumType [('BuyOrder_DatumType, 0)]

newtype ValidatorDatum
    = BuyOrder_Datum BuyOrder_DatumType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq ValidatorDatum where
    {-# INLINEABLE (==) #-}
    BuyOrder_Datum sd1 == BuyOrder_Datum sd2 = sd1 == sd2

PlutusTx.makeIsDataIndexed ''ValidatorDatum [('BuyOrder_Datum, 0)]

{-# INLINEABLE getBuyOrder_DatumType #-}
getBuyOrder_DatumType :: ValidatorDatum -> BuyOrder_DatumType
getBuyOrder_DatumType (BuyOrder_Datum sdType) = sdType

instance T.ShowDatum ValidatorDatum where
    showCborAsDatumType cbor = case LedgerApiV2.fromBuiltinData @ValidatorDatum cbor of
        Nothing -> Nothing
        Just d -> Just $ P.show d

--------------------------------------------------------------------------------2

{-# INLINEABLE mkBuyOrder_Datum #-}
mkBuyOrder_Datum :: T.CS -> T.CS -> T.WalletPaymentPKH -> Maybe T.WalletPaymentPKH -> Integer -> Integer -> Integer -> Integer -> LedgerValue.AssetClass -> Integer -> Integer -> ValidatorDatum
mkBuyOrder_Datum
    buyOrderPolicyID_CS
    fundPolicy_CS
    sellerPaymentPKH
    sellerStakePKH
    offeredCommission_InBPx1e3
    total_FT_Received
    total_FT_PayedAsCommission
    order_Status
    tokenMAYZ_AC
    requiredMAYZ
    minADA =
        BuyOrder_Datum $
            mkBuyOrder_DatumType
                buyOrderPolicyID_CS
                fundPolicy_CS
                sellerPaymentPKH
                sellerStakePKH
                offeredCommission_InBPx1e3
                total_FT_Received
                total_FT_PayedAsCommission
                order_Status
                tokenMAYZ_AC
                requiredMAYZ
                minADA

{-# INLINEABLE mkBuyOrder_DatumType #-}
mkBuyOrder_DatumType :: T.CS -> T.CS -> T.WalletPaymentPKH -> Maybe T.WalletPaymentPKH -> Integer -> Integer -> Integer -> Integer -> LedgerValue.AssetClass -> Integer -> Integer -> BuyOrder_DatumType
mkBuyOrder_DatumType = BuyOrder_DatumType ownVersion

mkDatum :: BuyOrder_DatumType -> LedgerApiV2.Datum
mkDatum = LedgerApiV2.Datum . LedgerApiV2.toBuiltinData . BuyOrder_Datum

--------------------------------------------------------------------------------2
-- PolicyRedeemer
--------------------------------------------------------------------------------2

data PolicyRedeemerMintIDType = PolicyRedeemerMintIDType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerMintIDType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemerMintIDType
    [('PolicyRedeemerMintIDType, 0)]

data PolicyRedeemerBurnIDType = PolicyRedeemerBurnIDType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerBurnIDType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemerBurnIDType
    [('PolicyRedeemerBurnIDType, 0)]

data PolicyRedeemer
    = PolicyRedeemerMintID PolicyRedeemerMintIDType
    | PolicyRedeemerBurnID PolicyRedeemerBurnIDType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemer where
    {-# INLINEABLE (==) #-}
    PolicyRedeemerMintID rmtx1 == PolicyRedeemerMintID rmtx2 = rmtx1 == rmtx2
    PolicyRedeemerBurnID rmtx1 == PolicyRedeemerBurnID rmtx2 = rmtx1 == rmtx2
    _ == _ = False

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemer
    [ ('PolicyRedeemerMintID, 1)
    , ('PolicyRedeemerBurnID, 2)
    ]

--------------------------------------------------------------------------------2

getPolicyRedeemerName :: Maybe PolicyRedeemer -> Maybe P.String
getPolicyRedeemerName (Just (PolicyRedeemerMintID PolicyRedeemerMintIDType)) = Just "MintID"
getPolicyRedeemerName (Just (PolicyRedeemerBurnID PolicyRedeemerBurnIDType)) = Just "BurnID"
getPolicyRedeemerName _ = Nothing

--------------------------------------------------------------------------------2
-- ValidatorRedeemer
--------------------------------------------------------------------------------2

newtype ValidatorRedeemerUpdateStatusType = ValidatorRedeemerUpdateStatusType {rusNewStatus :: Integer}
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerUpdateStatusType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = rusNewStatus r1 == rusNewStatus r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerUpdateStatusType [('ValidatorRedeemerUpdateStatusType, 0)]

newtype ValidatorRedeemerUpdateOfferedCommissionRateType = ValidatorRedeemerUpdateOfferedCommissionRateType {rucrNewCommissionRate :: Integer}
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerUpdateOfferedCommissionRateType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = rucrNewCommissionRate r1 == rucrNewCommissionRate r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerUpdateOfferedCommissionRateType [('ValidatorRedeemerUpdateOfferedCommissionRateType, 0)]

data ValidatorRedeemerUpdateMinADAType = ValidatorRedeemerUpdateMinADAType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerUpdateMinADAType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerUpdateMinADAType [('ValidatorRedeemerUpdateMinADAType, 0)]

newtype ValidatorRedeemerDepositType = ValidatorRedeemerDepositType {rdNewDeposit :: T.InvestUnit}
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerDepositType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = rdNewDeposit r1 == rdNewDeposit r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerDepositType [('ValidatorRedeemerDepositType, 0)]

newtype ValidatorRedeemerWithdrawType = ValidatorRedeemerWithdrawType {rwNewWithdraw :: T.InvestUnit}
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerWithdrawType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = rwNewWithdraw r1 == rwNewWithdraw r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerWithdrawType [('ValidatorRedeemerWithdrawType, 0)]

data ValidatorRedeemerFillOrderType = ValidatorRedeemerFillOrderType
    { rfoAmount_Tokens :: T.InvestUnit
    , rfoAmount_FT :: Integer
    , rfoCommission_FT :: Integer
    , rfoOracle_Data :: T.Oracle_Data
    , rfoOracle_Signature :: Ledger.Signature
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerFillOrderType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        rfoAmount_Tokens r1 == rfoAmount_Tokens r2
            && rfoAmount_FT r1 == rfoAmount_FT r2
            && rfoCommission_FT r1 == rfoCommission_FT r2
            && rfoOracle_Data r1 == rfoOracle_Data r2
            && rfoOracle_Signature r1 == rfoOracle_Signature r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerFillOrderType [('ValidatorRedeemerFillOrderType, 0)]

data ValidatorRedeemerDeleteType = ValidatorRedeemerDeleteType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerDeleteType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerDeleteType [('ValidatorRedeemerDeleteType, 0)]

data ValidatorRedeemer
    = ValidatorRedeemerUpdateStatus ValidatorRedeemerUpdateStatusType
    | ValidatorRedeemerUpdateOfferedCommissionRate ValidatorRedeemerUpdateOfferedCommissionRateType
    | ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType
    | ValidatorRedeemerDeposit ValidatorRedeemerDepositType
    | ValidatorRedeemerWithdraw ValidatorRedeemerWithdrawType
    | ValidatorRedeemerFillOrder ValidatorRedeemerFillOrderType
    | ValidatorRedeemerDelete ValidatorRedeemerDeleteType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemer where
    {-# INLINEABLE (==) #-}
    ValidatorRedeemerUpdateStatus rmf1 == ValidatorRedeemerUpdateStatus rmf2 = rmf1 == rmf2
    ValidatorRedeemerUpdateOfferedCommissionRate rmcp1 == ValidatorRedeemerUpdateOfferedCommissionRate rmcp2 = rmcp1 == rmcp2
    ValidatorRedeemerUpdateMinADA rmcp1 == ValidatorRedeemerUpdateMinADA rmcp2 = rmcp1 == rmcp2
    ValidatorRedeemerDeposit rmcp1 == ValidatorRedeemerDeposit rmcp2 = rmcp1 == rmcp2
    ValidatorRedeemerWithdraw rmcp1 == ValidatorRedeemerWithdraw rmcp2 = rmcp1 == rmcp2
    ValidatorRedeemerFillOrder rmcp1 == ValidatorRedeemerFillOrder rmcp2 = rmcp1 == rmcp2
    ValidatorRedeemerDelete rmcp1 == ValidatorRedeemerDelete rmcp2 = rmcp1 == rmcp2
    _ == _ = False

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemer
    [ ('ValidatorRedeemerUpdateStatus, 0)
    , ('ValidatorRedeemerUpdateOfferedCommissionRate, 1)
    , ('ValidatorRedeemerUpdateMinADA, 2)
    , ('ValidatorRedeemerDeposit, 3)
    , ('ValidatorRedeemerWithdraw, 4)
    , ('ValidatorRedeemerFillOrder, 5)
    , ('ValidatorRedeemerDelete, 6)
    ]

--------------------------------------------------------------------------------2

getValidatorRedeemerName :: Maybe ValidatorRedeemer -> Maybe P.String
getValidatorRedeemerName (Just (ValidatorRedeemerUpdateStatus (ValidatorRedeemerUpdateStatusType _))) = Just "UpdateStatus"
getValidatorRedeemerName (Just (ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType)) = Just "UpdateMinADA"
getValidatorRedeemerName (Just (ValidatorRedeemerDeposit ValidatorRedeemerDepositType {})) = Just "Deposit"
getValidatorRedeemerName (Just (ValidatorRedeemerWithdraw ValidatorRedeemerWithdrawType {})) = Just "Withdraw"
getValidatorRedeemerName (Just (ValidatorRedeemerFillOrder ValidatorRedeemerFillOrderType {})) = Just "FillOrder"
getValidatorRedeemerName (Just (ValidatorRedeemerDelete ValidatorRedeemerDeleteType)) = Just "Delete"
getValidatorRedeemerName _ = Nothing

--------------------------------------------------------------------------------2

mkMintIDRedeemer :: LedgerApiV2.Redeemer
mkMintIDRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            PolicyRedeemerMintID PolicyRedeemerMintIDType

mkBurnIDRedeemer :: LedgerApiV2.Redeemer
mkBurnIDRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            PolicyRedeemerBurnID PolicyRedeemerBurnIDType

--------------------------------------------------------------------------------2

mkUpdateStatusRedeemer :: Integer -> LedgerApiV2.Redeemer
mkUpdateStatusRedeemer newStatus =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerUpdateStatus $
                ValidatorRedeemerUpdateStatusType newStatus

mkUpdateOfferedCommissionRateRedeemer :: Integer -> LedgerApiV2.Redeemer
mkUpdateOfferedCommissionRateRedeemer newCommissionRate =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerUpdateOfferedCommissionRate $
                ValidatorRedeemerUpdateOfferedCommissionRateType newCommissionRate

mkUpdateMinADARedeemer :: LedgerApiV2.Redeemer
mkUpdateMinADARedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType

mkDepositRedeemer :: T.InvestUnit -> LedgerApiV2.Redeemer
mkDepositRedeemer newDeposit =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerDeposit $
                ValidatorRedeemerDepositType newDeposit

mkWithdrawRedeemer :: T.InvestUnit -> LedgerApiV2.Redeemer
mkWithdrawRedeemer newWithdraw =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerWithdraw $
                ValidatorRedeemerWithdrawType newWithdraw

mkFillOrderRedeemer :: T.InvestUnit -> Integer -> Integer -> T.Oracle_Data -> Ledger.Signature -> LedgerApiV2.Redeemer
mkFillOrderRedeemer amountTokens amountFT commissionFT oracleData oracleSignature =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerFillOrder $
                ValidatorRedeemerFillOrderType amountTokens amountFT commissionFT oracleData oracleSignature

mkDeleteRedeemer :: LedgerApiV2.Redeemer
mkDeleteRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerDelete ValidatorRedeemerDeleteType

--------------------------------------------------------------------------------2
