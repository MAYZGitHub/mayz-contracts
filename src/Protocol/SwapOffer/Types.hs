{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

module Protocol.SwapOffer.Types where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Data.Aeson             as DataAeson
import qualified Data.OpenApi.Schema    as DataOpenApiSchema
import qualified GHC.Generics           as GHCGenerics
import qualified Plutus.V2.Ledger.Api   as LedgerApiV2
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude                as P
import qualified Schema

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.OnChainHelpers as OnChainHelpers
import qualified Generic.Types          as T
import qualified Ledger
import qualified Protocol.Types         as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

--------------------------------------------------------------------------------2
-- Params
--------------------------------------------------------------------------------2

data PolicyParams
    = PolicyParams
          { ppProtocolPolicyID_CS      :: T.CS
          , ppSwapOffer_Validator_Hash :: LedgerApiV2.ValidatorHash
          , ppTokenMAYZ_AC             :: Ledger.AssetClass
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq PolicyParams where
    {-# INLINEABLE (==) #-}
    p1 == p2 =
        ppProtocolPolicyID_CS p1 == ppProtocolPolicyID_CS p2
            && ppSwapOffer_Validator_Hash p1 == ppSwapOffer_Validator_Hash p2
            && ppTokenMAYZ_AC p1 == ppTokenMAYZ_AC p2

PlutusTx.makeLift ''PolicyParams
PlutusTx.makeIsDataIndexed ''PolicyParams [('PolicyParams, 0)]

data ValidatorParams
    = ValidatorParams
          { vpProtocolPolicyID_CS          :: T.CS
          , vpTokenEmergencyAdminPolicy_CS :: LedgerApiV2.CurrencySymbol
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq ValidatorParams where
    {-# INLINEABLE (==) #-}
    p1 == p2 =
        vpProtocolPolicyID_CS p1 == vpProtocolPolicyID_CS p2
            && vpTokenEmergencyAdminPolicy_CS p1 == vpTokenEmergencyAdminPolicy_CS p2

PlutusTx.makeLift ''ValidatorParams
PlutusTx.makeIsDataIndexed ''ValidatorParams [('ValidatorParams, 0)]

--------------------------------------------------------------------------------2
-- Datums
--------------------------------------------------------------------------------2

data SwapOffer_DatumType
    = SwapOffer_DatumType
          { sodSwapOfferPolicyID_CS     :: T.CS
          , sodFundPolicy_CS            :: T.CS
          , sodSellerPaymentPKH         :: T.WalletPaymentPKH
          , sodSellerStakePKH           :: Maybe T.WalletPaymentPKH
          , sodAskedCommission_InBPx1e3 :: Integer
          , sodAmount_FT_Available      :: Integer
          , sodAmount_ADA_Available     :: Integer
          , sodTotal_FT_Earned          :: Integer
          , sodTotal_ADA_Earned         :: Integer
          , sodOrder_AllowSellFT        :: Integer
          , sodOrder_AllowSellADA       :: Integer
          , sodOrder_Status             :: Integer
          , sodMAYZ                     :: Integer
          , sodMinADA                   :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq SwapOffer_DatumType where
    {-# INLINEABLE (==) #-}
    sd1 == sd2 =
        sodSwapOfferPolicyID_CS sd1 == sodSwapOfferPolicyID_CS sd2
            && sodFundPolicy_CS sd1 == sodFundPolicy_CS sd2
            && sodSellerPaymentPKH sd1 == sodSellerPaymentPKH sd2
            && sodSellerStakePKH sd1 == sodSellerStakePKH sd2
            && sodAskedCommission_InBPx1e3 sd1 == sodAskedCommission_InBPx1e3 sd2
            && sodAmount_FT_Available sd1 == sodAmount_FT_Available sd2
            && sodAmount_ADA_Available sd1 == sodAmount_ADA_Available sd2
            && sodTotal_FT_Earned sd1 == sodTotal_FT_Earned sd2
            && sodTotal_ADA_Earned sd1 == sodTotal_ADA_Earned sd2
            && sodOrder_AllowSellFT sd1 == sodOrder_AllowSellFT sd2
            && sodOrder_AllowSellADA sd1 == sodOrder_AllowSellADA sd2
            && sodOrder_Status sd1 == sodOrder_Status sd2
            && sodMAYZ sd1 == sodMAYZ sd2
            && sodMinADA sd1 == sodMinADA sd2

PlutusTx.makeIsDataIndexed ''SwapOffer_DatumType [('SwapOffer_DatumType, 0)]

newtype ValidatorDatum
    = SwapOffer_Datum SwapOffer_DatumType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq ValidatorDatum where
    {-# INLINEABLE (==) #-}
    SwapOffer_Datum sd1 == SwapOffer_Datum sd2 = sd1 == sd2

PlutusTx.makeIsDataIndexed ''ValidatorDatum [('SwapOffer_Datum, 0)]

{-# INLINEABLE getSwapOffer_DatumType #-}
getSwapOffer_DatumType :: ValidatorDatum -> SwapOffer_DatumType
getSwapOffer_DatumType (SwapOffer_Datum sdType) = sdType

{-# INLINEABLE getSwapOffer_DatumType_From_UTxO #-}
getSwapOffer_DatumType_From_UTxO :: LedgerApiV2.TxOut -> SwapOffer_DatumType
getSwapOffer_DatumType_From_UTxO utxo = case OnChainHelpers.getInlineDatum_From_TxOut @ValidatorDatum utxo of
                    Nothing     -> P.error "No SwapOffer Datum found"
                    Just datum' -> getSwapOffer_DatumType datum'

instance T.ShowDatum ValidatorDatum where
    showCborAsDatumType cbor = case LedgerApiV2.fromBuiltinData @ValidatorDatum cbor of
        Nothing -> Nothing
        Just d  -> Just $ P.show d

--------------------------------------------------------------------------------2

{-# INLINEABLE mkSwapOffer_DatumType #-}
mkSwapOffer_DatumType :: T.CS -> T.CS -> T.WalletPaymentPKH -> Maybe T.WalletPaymentPKH -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> SwapOffer_DatumType
mkSwapOffer_DatumType = SwapOffer_DatumType

{-# INLINEABLE mkSwapOffer_Datum #-}
mkSwapOffer_Datum :: T.CS -> T.CS -> T.WalletPaymentPKH -> Maybe T.WalletPaymentPKH -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> ValidatorDatum
mkSwapOffer_Datum
    swapOfferPolicyID_CS
    fundPolicy_CS
    sellerPaymentPKH
    sellerStakePKH
    askedCommission_InBPx1e3
    amount_FT_Available
    amount_ADA_Available
    total_FT_Earned
    total_ADA_Earned
    allowSellFT
    allowSellADA
    order_Status
    amountMAYZ
    minADA =
        SwapOffer_Datum $
            mkSwapOffer_DatumType
                swapOfferPolicyID_CS
                fundPolicy_CS
                sellerPaymentPKH
                sellerStakePKH
                askedCommission_InBPx1e3
                amount_FT_Available
                amount_ADA_Available
                total_FT_Earned
                total_ADA_Earned
                allowSellFT
                allowSellADA
                order_Status
                amountMAYZ
                minADA

mkDatum :: SwapOffer_DatumType -> LedgerApiV2.Datum
mkDatum = LedgerApiV2.Datum . LedgerApiV2.toBuiltinData . SwapOffer_Datum

--------------------------------------------------------------------------------2
-- PolicyRedeemer
--------------------------------------------------------------------------------2

data PolicyRedeemerMintIDType = PolicyRedeemerMintIDType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerMintIDType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.unstableMakeIsData ''PolicyRedeemerMintIDType

data PolicyRedeemerBurnIDType = PolicyRedeemerBurnIDType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerBurnIDType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.unstableMakeIsData ''PolicyRedeemerBurnIDType

data PolicyRedeemer
    = PolicyRedeemerMintID PolicyRedeemerMintIDType
    | PolicyRedeemerBurnID PolicyRedeemerBurnIDType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemer where
    {-# INLINEABLE (==) #-}
    PolicyRedeemerMintID rmtx1 == PolicyRedeemerMintID rmtx2 = rmtx1 == rmtx2
    PolicyRedeemerBurnID rmtx1 == PolicyRedeemerBurnID rmtx2 = rmtx1 == rmtx2
    _ == _                                                   = False

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemer
    [ ('PolicyRedeemerMintID, 1)
    , ('PolicyRedeemerBurnID, 2)
    ]

--------------------------------------------------------------------------------2

getPolicyRedeemerName :: Maybe PolicyRedeemer -> Maybe P.String
getPolicyRedeemerName (Just (PolicyRedeemerMintID PolicyRedeemerMintIDType)) = Just "MintID"
getPolicyRedeemerName (Just (PolicyRedeemerBurnID PolicyRedeemerBurnIDType)) = Just "BurnID"
getPolicyRedeemerName _                                                      = Nothing

--------------------------------------------------------------------------------2
-- ValidatorRedeemer
--------------------------------------------------------------------------------2

newtype ValidatorRedeemerUpdateStatusType
    = ValidatorRedeemerUpdateStatusType { rusNewStatus :: Integer }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerUpdateStatusType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = rusNewStatus r1 == rusNewStatus r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerUpdateStatusType [('ValidatorRedeemerUpdateStatusType, 0)]

newtype ValidatorRedeemerUpdateAskedCommissionRateType
    = ValidatorRedeemerUpdateAskedCommissionRateType { rucrNewCommissionRate :: Integer }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerUpdateAskedCommissionRateType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = rucrNewCommissionRate r1 == rucrNewCommissionRate r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerUpdateAskedCommissionRateType [('ValidatorRedeemerUpdateAskedCommissionRateType, 0)]

data ValidatorRedeemerUpdateSellRestrictionsType
    = ValidatorRedeemerUpdateSellRestrictionsType
          { rusrAllowSellFT  :: Integer
          , rusrAllowSellADA :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerUpdateSellRestrictionsType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        rusrAllowSellFT r1 == rusrAllowSellFT r2
            && rusrAllowSellADA r1 == rusrAllowSellADA r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerUpdateSellRestrictionsType [('ValidatorRedeemerUpdateSellRestrictionsType, 0)]

data ValidatorRedeemerUpdateMinADAType = ValidatorRedeemerUpdateMinADAType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerUpdateMinADAType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerUpdateMinADAType [('ValidatorRedeemerUpdateMinADAType, 0)]

data ValidatorRedeemerDepositType
    = ValidatorRedeemerDepositType
          { rdNewDeposit_FT  :: Integer
          , rdNewDeposit_ADA :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerDepositType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        rdNewDeposit_FT r1 == rdNewDeposit_FT r2
            && rdNewDeposit_ADA r1 == rdNewDeposit_ADA r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerDepositType [('ValidatorRedeemerDepositType, 0)]

data ValidatorRedeemerWithdrawType
    = ValidatorRedeemerWithdrawType
          { rwNewWithdraw_FT  :: Integer
          , rwNewWithdraw_ADA :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerWithdrawType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        rwNewWithdraw_FT r1 == rwNewWithdraw_FT r2
            && rwNewWithdraw_ADA r1 == rwNewWithdraw_ADA r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerWithdrawType [('ValidatorRedeemerWithdrawType, 0)]

data ValidatorRedeemerSwapFTxADAType
    = ValidatorRedeemerSwapFTxADAType
          { rsfxaAmount_FT        :: Integer
          , rsfxaAmount_ADA       :: Integer
          , rsfxaCommission_ADA   :: Integer
          , rsfxaOracle_Data      :: T.Oracle_Data
          , rsfxaOracle_Signature :: Ledger.Signature
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerSwapFTxADAType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        rsfxaAmount_FT r1 == rsfxaAmount_FT r2
            && rsfxaAmount_ADA r1 == rsfxaAmount_ADA r2
            && rsfxaCommission_ADA r1 == rsfxaCommission_ADA r2
            && rsfxaOracle_Data r1 == rsfxaOracle_Data r2
            && rsfxaOracle_Signature r1 == rsfxaOracle_Signature r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerSwapFTxADAType [('ValidatorRedeemerSwapFTxADAType, 0)]

data ValidatorRedeemerSwapADAxFTType
    = ValidatorRedeemerSwapADAxFTType
          { rsaxfAmount_ADA       :: Integer
          , rsaxfAmount_FT        :: Integer
          , rsaxfCommission_FT    :: Integer
          , rsaxfOracle_Data      :: T.Oracle_Data
          , rsaxfOracle_Signature :: Ledger.Signature
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerSwapADAxFTType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        rsaxfAmount_ADA r1 == rsaxfAmount_ADA r2
            && rsaxfAmount_FT r1 == rsaxfAmount_FT r2
            && rsaxfCommission_FT r1 == rsaxfCommission_FT r2
            && rsaxfOracle_Data r1 == rsaxfOracle_Data r2
            && rsaxfOracle_Signature r1 == rsaxfOracle_Signature r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerSwapADAxFTType [('ValidatorRedeemerSwapADAxFTType, 0)]

data ValidatorRedeemerDeleteType = ValidatorRedeemerDeleteType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerDeleteType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerDeleteType [('ValidatorRedeemerDeleteType, 0)]

data ValidatorRedeemerEmergencyType = ValidatorRedeemerEmergencyType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerEmergencyType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerEmergencyType
    [('ValidatorRedeemerEmergencyType, 0)]

--------------------------------------------------------------------------------2

data ValidatorRedeemer
    = ValidatorRedeemerUpdateStatus ValidatorRedeemerUpdateStatusType
    | ValidatorRedeemerUpdateAskedCommissionRate ValidatorRedeemerUpdateAskedCommissionRateType
    | ValidatorRedeemerUpdateSellRestrictions ValidatorRedeemerUpdateSellRestrictionsType
    | ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType
    | ValidatorRedeemerDeposit ValidatorRedeemerDepositType
    | ValidatorRedeemerWithdraw ValidatorRedeemerWithdrawType
    | ValidatorRedeemerSwapFTxADA ValidatorRedeemerSwapFTxADAType
    | ValidatorRedeemerSwapADAxFT ValidatorRedeemerSwapADAxFTType
    | ValidatorRedeemerDelete ValidatorRedeemerDeleteType
    | ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemer where
    {-# INLINEABLE (==) #-}
    ValidatorRedeemerUpdateStatus rmf1 == ValidatorRedeemerUpdateStatus rmf2                             = rmf1 == rmf2
    ValidatorRedeemerUpdateAskedCommissionRate rmcp1 == ValidatorRedeemerUpdateAskedCommissionRate rmcp2 = rmcp1 == rmcp2
    ValidatorRedeemerUpdateSellRestrictions rmcp1 == ValidatorRedeemerUpdateSellRestrictions rmcp2       = rmcp1 == rmcp2
    ValidatorRedeemerUpdateMinADA rmcp1 == ValidatorRedeemerUpdateMinADA rmcp2                           = rmcp1 == rmcp2
    ValidatorRedeemerDeposit rmcp1 == ValidatorRedeemerDeposit rmcp2                                     = rmcp1 == rmcp2
    ValidatorRedeemerWithdraw rmcp1 == ValidatorRedeemerWithdraw rmcp2                                   = rmcp1 == rmcp2
    ValidatorRedeemerSwapFTxADA rmcp1 == ValidatorRedeemerSwapFTxADA rmcp2                               = rmcp1 == rmcp2
    ValidatorRedeemerSwapADAxFT rmcp1 == ValidatorRedeemerSwapADAxFT rmcp2                               = rmcp1 == rmcp2
    ValidatorRedeemerDelete rmcp1 == ValidatorRedeemerDelete rmcp2                                       = rmcp1 == rmcp2
    ValidatorRedeemerEmergency rmcp1 == ValidatorRedeemerEmergency rmcp2                                 = rmcp1 == rmcp2
    _ == _                                                                                               = False

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemer
    [ ('ValidatorRedeemerUpdateStatus, 0)
    , ('ValidatorRedeemerUpdateAskedCommissionRate, 1)
    , ('ValidatorRedeemerUpdateSellRestrictions, 2)
    , ('ValidatorRedeemerUpdateMinADA, 3)
    , ('ValidatorRedeemerDeposit, 4)
    , ('ValidatorRedeemerWithdraw, 5)
    , ('ValidatorRedeemerSwapFTxADA, 6)
    , ('ValidatorRedeemerSwapADAxFT, 7)
    , ('ValidatorRedeemerDelete, 8)
    , ('ValidatorRedeemerEmergency, 9)
    ]

--------------------------------------------------------------------------------2

getValidatorRedeemerName :: Maybe ValidatorRedeemer -> Maybe P.String
getValidatorRedeemerName (Just (ValidatorRedeemerUpdateStatus (ValidatorRedeemerUpdateStatusType _)))                           = Just "UpdateStatus"
getValidatorRedeemerName (Just (ValidatorRedeemerUpdateAskedCommissionRate (ValidatorRedeemerUpdateAskedCommissionRateType _))) = Just "UpdateAskedCommissionRate"
getValidatorRedeemerName (Just (ValidatorRedeemerUpdateSellRestrictions (ValidatorRedeemerUpdateSellRestrictionsType _ _)))     = Just "UpdateSellRestrictions"
getValidatorRedeemerName (Just (ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType))                               = Just "UpdateMinADA"
getValidatorRedeemerName (Just (ValidatorRedeemerDeposit (ValidatorRedeemerDepositType _ _)))                                   = Just "Deposit"
getValidatorRedeemerName (Just (ValidatorRedeemerWithdraw (ValidatorRedeemerWithdrawType _ _)))                                 = Just "Withdraw"
getValidatorRedeemerName (Just (ValidatorRedeemerSwapFTxADA ValidatorRedeemerSwapFTxADAType {}))                                = Just "SwapFTxADA"
getValidatorRedeemerName (Just (ValidatorRedeemerSwapADAxFT ValidatorRedeemerSwapADAxFTType {}))                                = Just "SwapADAxFT"
getValidatorRedeemerName (Just (ValidatorRedeemerDelete ValidatorRedeemerDeleteType))                                           = Just "Delete"
getValidatorRedeemerName (Just (ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType))                                     = Just "Emergency"
getValidatorRedeemerName _                                                                                                      = Nothing

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
            ValidatorRedeemerUpdateStatus $ ValidatorRedeemerUpdateStatusType newStatus

mkUpdateAskedCommissionRateRedeemer :: Integer -> LedgerApiV2.Redeemer
mkUpdateAskedCommissionRateRedeemer newCommissionRate =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerUpdateAskedCommissionRate $ ValidatorRedeemerUpdateAskedCommissionRateType newCommissionRate

mkUpdateSellRestrictionsRedeemer :: Integer -> Integer -> LedgerApiV2.Redeemer
mkUpdateSellRestrictionsRedeemer newAllowSellFT newAllowSellADA =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerUpdateSellRestrictions $
                ValidatorRedeemerUpdateSellRestrictionsType newAllowSellFT newAllowSellADA

mkUpdateMinADARedeemer :: LedgerApiV2.Redeemer
mkUpdateMinADARedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType

mkDepositRedeemer :: Integer -> Integer -> LedgerApiV2.Redeemer
mkDepositRedeemer newDeposit_FT newDeposit_ADA =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerDeposit $
                ValidatorRedeemerDepositType newDeposit_FT newDeposit_ADA

mkWithdrawRedeemer :: Integer -> Integer -> LedgerApiV2.Redeemer
mkWithdrawRedeemer newWithdraw_FT newWithdraw_ADA =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerWithdraw $
                ValidatorRedeemerWithdrawType newWithdraw_FT newWithdraw_ADA

mkSwapFTxADARedeemer :: Integer -> Integer -> Integer -> T.Oracle_Data -> Ledger.Signature -> LedgerApiV2.Redeemer
mkSwapFTxADARedeemer amount_FT amount_ADA commission_ADA oracleData oracleSignature =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerSwapFTxADA $
                ValidatorRedeemerSwapFTxADAType amount_FT amount_ADA commission_ADA oracleData oracleSignature

mkSwapADAxFTRedeemer :: Integer -> Integer -> Integer -> T.Oracle_Data -> Ledger.Signature -> LedgerApiV2.Redeemer
mkSwapADAxFTRedeemer amount_ADA amount_FT commission_FT oracleData oracleSignature =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerSwapADAxFT $
                ValidatorRedeemerSwapADAxFTType amount_ADA amount_FT commission_FT oracleData oracleSignature

mkDeleteRedeemer :: LedgerApiV2.Redeemer
mkDeleteRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerDelete ValidatorRedeemerDeleteType

mkEmergencyRedeemer :: LedgerApiV2.Redeemer
mkEmergencyRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType

--------------------------------------------------------------------------------2
