{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
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

module Protocol.Fund.Types where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Data.Aeson             as DataAeson (FromJSON, ToJSON)
import qualified Data.OpenApi.Schema    as DataOpenApiSchema (ToSchema)
import qualified GHC.Generics           as GHCGenerics (Generic)
import qualified Ledger
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
import qualified Protocol.Constants     as T
import qualified Protocol.Types         as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

--------------------------------------------------------------------------------2
-- Params
--------------------------------------------------------------------------------2

data PolicyParams
    = PolicyParams
          { ppProtocolPolicyID_CS :: LedgerApiV2.CurrencySymbol
          , ppFundPolicy_TxOutRef :: LedgerApiV2.TxOutRef
          , ppFundValidator_Hash  :: LedgerApiV2.ValidatorHash
          , ppTokenMAYZ_AC        :: Ledger.AssetClass
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq PolicyParams where
    {-# INLINABLE (==) #-}
    pp1 == pp2 =
        ppProtocolPolicyID_CS pp1 == ppProtocolPolicyID_CS pp2
            && ppFundPolicy_TxOutRef pp1 == ppFundPolicy_TxOutRef pp2
            && ppFundValidator_Hash pp1 == ppFundValidator_Hash pp2
            && ppTokenMAYZ_AC pp1 == ppTokenMAYZ_AC pp2

PlutusTx.makeLift ''PolicyParams

PlutusTx.makeIsDataIndexed ''PolicyParams [('PolicyParams, 0)]

data ValidatorParams
    = ValidatorParams
          { vpProtocolPolicyID_CS          :: LedgerApiV2.CurrencySymbol
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

data FundDatumType
    = FundDatumType
          { fdFundFactoryVersion            :: Integer
          , fdFundPolicy_CS                 :: T.CS
          , fdFundFT_TN                     :: T.TN
          , fdFundValidator_Hash            :: LedgerApiV2.ValidatorHash
          , fdFundHoldingPolicyID_CS        :: T.CS
          , fdFundHoldingValidator_Hash     :: LedgerApiV2.ValidatorHash
          , fdInvestUnitValidator_Hash      :: LedgerApiV2.ValidatorHash
          , fdAdmins                        :: [T.WalletPaymentPKH]
          , fdTokenAdminPolicy_CS           :: LedgerApiV2.CurrencySymbol
          , fdFundCategoryNumber            :: Integer
          , fdBeginAt                       :: LedgerApiV2.POSIXTime
          , fdDeadline                      :: LedgerApiV2.POSIXTime
          , fdClosedAt                      :: Maybe LedgerApiV2.POSIXTime
          , fdCommissionPerYearInBPx1e3     :: Integer
          , fdCommissionsTable_Numerator1e6 :: [Integer]
          , fdHoldingsCount                 :: Integer
          , fdHoldingsIndex                 :: Integer
          , fdMAYZ                          :: Integer
          , fdMinADA                        :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq FundDatumType where
  {-# INLINABLE (==) #-}
  ps1 == ps2 =
    fdFundFactoryVersion ps1 == fdFundFactoryVersion ps2
        && fdFundPolicy_CS ps1 == fdFundPolicy_CS ps2
        && fdFundFT_TN ps1 == fdFundFT_TN ps2
        && fdFundValidator_Hash ps1 == fdFundValidator_Hash ps2
        && fdFundHoldingPolicyID_CS ps1 == fdFundHoldingPolicyID_CS ps2
        && fdFundHoldingValidator_Hash ps1 == fdFundHoldingValidator_Hash ps2
        && fdInvestUnitValidator_Hash ps1 == fdInvestUnitValidator_Hash ps2
        && fdAdmins ps1 == fdAdmins ps2
        && fdTokenAdminPolicy_CS ps1 == fdTokenAdminPolicy_CS ps2
        && fdFundCategoryNumber ps1 == fdFundCategoryNumber ps2
        && fdBeginAt ps1 == fdBeginAt ps2
        && fdDeadline ps1 == fdDeadline ps2
        && fdClosedAt ps1 == fdClosedAt ps2
        && fdCommissionPerYearInBPx1e3 ps1 == fdCommissionPerYearInBPx1e3 ps2
        && fdCommissionsTable_Numerator1e6 ps1 == fdCommissionsTable_Numerator1e6 ps2
        && fdHoldingsCount ps1 == fdHoldingsCount ps2
        && fdHoldingsIndex ps1 == fdHoldingsIndex ps2
        && fdMAYZ ps1 == fdMAYZ ps2
        && fdMinADA ps1 == fdMinADA ps2

instance T.HasAdmins FundDatumType where
    {-# INLINABLE getAdmins #-}
    getAdmins = fdAdmins

instance T.HasAdminToken FundDatumType where
    {-# INLINABLE getAdminToken_CS #-}
    getAdminToken_CS = fdTokenAdminPolicy_CS

PlutusTx.makeIsDataIndexed ''FundDatumType [('FundDatumType, 0)]

--------------------------------------------------------------------------------2
newtype ValidatorDatum
    = FundDatum FundDatumType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq ValidatorDatum where
    {-# INLINABLE (==) #-}
    FundDatum mps1 == FundDatum mps2 = mps1 == mps2

PlutusTx.makeIsDataIndexed ''ValidatorDatum [('FundDatum, 0)]

{-# INLINABLE getFund_DatumType #-}
getFund_DatumType :: ValidatorDatum -> FundDatumType
getFund_DatumType (FundDatum sdType) = sdType

{-# INLINEABLE getFund_DatumType_From_UTxO #-}
getFund_DatumType_From_UTxO :: LedgerApiV2.TxOut -> FundDatumType
getFund_DatumType_From_UTxO utxo = case OnChainHelpers.getInlineDatum_From_TxOut @ValidatorDatum utxo of
                    Nothing     -> P.error "No Fund Datum found"
                    Just datum' -> getFund_DatumType datum'

instance T.ShowDatum ValidatorDatum where
  showCborAsDatumType cbor =
    case LedgerApiV2.fromBuiltinData @ValidatorDatum cbor of
        Nothing -> Nothing
        Just d  -> Just $ P.show d

--------------------------------------------------------------------------------2

{-# INLINABLE mkFund_DatumType #-}
mkFund_DatumType ::
        T.CS
    -> T.TN
    -> LedgerApiV2.ValidatorHash
    -> T.CS
    -> LedgerApiV2.ValidatorHash
    -> LedgerApiV2.ValidatorHash
    -> [T.WalletPaymentPKH]
    -> LedgerApiV2.CurrencySymbol
    -> Integer
    -> LedgerApiV2.POSIXTime
    -> LedgerApiV2.POSIXTime
    -> Maybe LedgerApiV2.POSIXTime
    -> Integer
    -> [Integer]
    -> Integer
    -> Integer
    -> Integer
    -> Integer
    -> FundDatumType
mkFund_DatumType
    fundPolicy_CS
    fundFT_TN
    fundValidator_Hash
    fundHoldingPolicyID_CS
    fundHoldingValidator_Hash
    investUnitValidator_Hash
    admins
    tokenAdminPolicy_CS
    fundCategoryNumber
    beginAt
    deadline
    closedAt
    commissionPerYearInBPx1e3
    commissionsTable_Numerator1e6
    holdingsCount
    holdingsIndex
    amountMAYZ
    minADA =
    let !adminsOrdered = sort admins
    in  FundDatumType
            {
            fdFundFactoryVersion = T.fundFactoryVersion
            , fdFundPolicy_CS = fundPolicy_CS
            , fdFundFT_TN = fundFT_TN
            , fdFundValidator_Hash = fundValidator_Hash
            , fdFundHoldingPolicyID_CS = fundHoldingPolicyID_CS
            , fdFundHoldingValidator_Hash = fundHoldingValidator_Hash
            , fdInvestUnitValidator_Hash = investUnitValidator_Hash
            , fdAdmins = adminsOrdered
            , fdTokenAdminPolicy_CS = tokenAdminPolicy_CS
            , fdFundCategoryNumber = fundCategoryNumber
            , fdBeginAt = beginAt
            , fdDeadline = deadline
            , fdClosedAt = closedAt
            , fdCommissionPerYearInBPx1e3 = commissionPerYearInBPx1e3
            , fdCommissionsTable_Numerator1e6 = commissionsTable_Numerator1e6
            , fdHoldingsCount = holdingsCount
            , fdHoldingsIndex = holdingsIndex
            , fdMAYZ = amountMAYZ
            , fdMinADA = minADA
            }

{-# INLINABLE mkFund_Datum #-}
mkFund_Datum ::
        T.CS
    -> T.TN
    -> LedgerApiV2.ValidatorHash
    -> T.CS
    -> LedgerApiV2.ValidatorHash
    -> LedgerApiV2.ValidatorHash
    -> [T.WalletPaymentPKH]
    -> LedgerApiV2.CurrencySymbol
    -> Integer
    -> LedgerApiV2.POSIXTime
    -> LedgerApiV2.POSIXTime
    -> Maybe LedgerApiV2.POSIXTime
    -> Integer
    -> [Integer]
    -> Integer
    -> Integer
    -> Integer
    -> Integer
    -> ValidatorDatum
mkFund_Datum
    fundPolicy_CS
    fundFT_TN
    fundValidator_Hash
    fundHoldingPolicyID_CS
    fundHoldingValidator_Hash
    investUnitValidator_Hash
    admins
    tokenAdminPolicy_CS
    fundCategoryNumber
    beginAt
    deadline
    closedAt
    commissionPerYearInBPx1e3
    commissionsTable_Numerator1e6
    holdingsCount
    holdingsIndex
    amountMAYZ
    minADA =
  FundDatum
    $ mkFund_DatumType
        fundPolicy_CS
        fundFT_TN
        fundValidator_Hash
        fundHoldingPolicyID_CS
        fundHoldingValidator_Hash
        investUnitValidator_Hash
        admins
        tokenAdminPolicy_CS
        fundCategoryNumber
        beginAt
        deadline
        closedAt
        commissionPerYearInBPx1e3
        commissionsTable_Numerator1e6
        holdingsCount
        holdingsIndex
        amountMAYZ
        minADA

mkDatum :: FundDatumType -> LedgerApiV2.Datum
mkDatum = LedgerApiV2.Datum . LedgerApiV2.toBuiltinData . FundDatum

--------------------------------------------------------------------------------2
-- PolicyRedeemer
--------------------------------------------------------------------------------2
data PolicyRedeemerMintIDType = PolicyRedeemerMintIDType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerMintIDType where
    {-# INLINABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemerMintIDType
    [('PolicyRedeemerMintIDType, 0)]

data PolicyRedeemerBurnIDType = PolicyRedeemerBurnIDType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerBurnIDType where
    {-# INLINABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemerBurnIDType
    [('PolicyRedeemerBurnIDType, 0)]

data PolicyRedeemerMintFTType = PolicyRedeemerMintFTType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerMintFTType where
    {-# INLINABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemerMintFTType
    [('PolicyRedeemerMintFTType, 0)]

data PolicyRedeemerBurnFTType = PolicyRedeemerBurnFTType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerBurnFTType where
    {-# INLINABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemerBurnFTType
    [('PolicyRedeemerBurnFTType, 0)]

data PolicyRedeemer
    = PolicyRedeemerMintID PolicyRedeemerMintIDType
    | PolicyRedeemerBurnID PolicyRedeemerBurnIDType
    | PolicyRedeemerMintFT PolicyRedeemerMintFTType
    | PolicyRedeemerBurnFT PolicyRedeemerBurnFTType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemer where
    {-# INLINABLE (==) #-}
    PolicyRedeemerMintID rmtx1 == PolicyRedeemerMintID rmtx2 = rmtx1 == rmtx2
    PolicyRedeemerBurnID rmtx1 == PolicyRedeemerBurnID rmtx2 = rmtx1 == rmtx2
    PolicyRedeemerMintFT rmtx1 == PolicyRedeemerMintFT rmtx2 = rmtx1 == rmtx2
    PolicyRedeemerBurnFT rmtx1 == PolicyRedeemerBurnFT rmtx2 = rmtx1 == rmtx2
    _ == _                                                   = False

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemer
    [ ('PolicyRedeemerMintID, 0)
    , ('PolicyRedeemerBurnID, 1)
    , ('PolicyRedeemerMintFT, 2)
    , ('PolicyRedeemerBurnFT, 3)
    ]


--------------------------------------------------------------------------------2

getPolicyRedeemerName :: Maybe PolicyRedeemer -> Maybe P.String
getPolicyRedeemerName (Just (PolicyRedeemerMintID PolicyRedeemerMintIDType)) = Just "MintID"
getPolicyRedeemerName (Just (PolicyRedeemerBurnID PolicyRedeemerBurnIDType)) = Just "BurnID"
getPolicyRedeemerName (Just (PolicyRedeemerMintFT PolicyRedeemerMintFTType)) = Just "MintFT"
getPolicyRedeemerName (Just (PolicyRedeemerBurnFT PolicyRedeemerBurnFTType)) = Just "BurnFT"
getPolicyRedeemerName _                                                      = Nothing

--------------------------------------------------------------------------------2
-- ValidatorRedeemer
--------------------------------------------------------------------------------2
data ValidatorRedeemerDatumUpdateType = ValidatorRedeemerDatumUpdateType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerDatumUpdateType where
    {-# INLINABLE (==) #-}
    r1 == r2 = r1 == r2

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
data ValidatorRedeemerFundHoldingAddType = ValidatorRedeemerFundHoldingAddType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerFundHoldingAddType where
    {-# INLINABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerFundHoldingAddType
    [('ValidatorRedeemerFundHoldingAddType, 0)]

--------------------------------------------------------------------------------2
data ValidatorRedeemerFundHoldingDeleteType = ValidatorRedeemerFundHoldingDeleteType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerFundHoldingDeleteType where
    {-# INLINABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerFundHoldingDeleteType
    [('ValidatorRedeemerFundHoldingDeleteType, 0)]

--------------------------------------------------------------------------------2
data ValidatorRedeemerEmergencyType = ValidatorRedeemerEmergencyType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerEmergencyType where
    {-# INLINABLE (==) #-}
    r1 == r2 =  r1 ==  r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerEmergencyType
    [('ValidatorRedeemerEmergencyType, 0)]

--------------------------------------------------------------------------------2
newtype ValidatorRedeemerFinishType
    = ValidatorRedeemerFinishType { rfDate :: LedgerApiV2.POSIXTime }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerFinishType where
    {-# INLINABLE (==) #-}
    r1 == r2 = rfDate r1 == rfDate r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerFinishType
    [('ValidatorRedeemerFinishType, 0)]

--------------------------------------------------------------------------------2
data ValidatorRedeemerDeleteType = ValidatorRedeemerDeleteType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerDeleteType where
    {-# INLINABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerDeleteType
    [('ValidatorRedeemerDeleteType, 0)]

--------------------------------------------------------------------------------2
data ValidatorRedeemer
    = ValidatorRedeemerDatumUpdate ValidatorRedeemerDatumUpdateType
    | ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType
    | ValidatorRedeemerFundHoldingAdd ValidatorRedeemerFundHoldingAddType
    | ValidatorRedeemerFundHoldingDelete ValidatorRedeemerFundHoldingDeleteType
    | ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType
    | ValidatorRedeemerFinish ValidatorRedeemerFinishType
    | ValidatorRedeemerDelete ValidatorRedeemerDeleteType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemer where
    {-# INLINABLE (==) #-}
    (==) :: ValidatorRedeemer -> ValidatorRedeemer -> Bool
    ValidatorRedeemerDatumUpdate rmf1 == ValidatorRedeemerDatumUpdate rmf2 =
        rmf1 == rmf2
    ValidatorRedeemerUpdateMinADA rmcp1 == ValidatorRedeemerUpdateMinADA rmcp2 =
        rmcp1 == rmcp2
    ValidatorRedeemerFundHoldingAdd rmcp1 == ValidatorRedeemerFundHoldingAdd rmcp2 =
        rmcp1 == rmcp2
    ValidatorRedeemerFundHoldingDelete rmcp1 == ValidatorRedeemerFundHoldingDelete rmcp2 =
        rmcp1 == rmcp2
    ValidatorRedeemerEmergency rmcp1 == ValidatorRedeemerEmergency rmcp2                 = rmcp1 == rmcp2
    ValidatorRedeemerFinish rmcp1 == ValidatorRedeemerFinish rmcp2 =
        rmcp1 == rmcp2
    ValidatorRedeemerDelete rmcp1 == ValidatorRedeemerDelete rmcp2 =
        rmcp1 == rmcp2
    _ == _ = False

PlutusTx.makeIsDataIndexed
  ''ValidatorRedeemer
    [ ('ValidatorRedeemerDatumUpdate, 0)
    , ('ValidatorRedeemerUpdateMinADA, 1)
    , ('ValidatorRedeemerFundHoldingAdd, 2)
    , ('ValidatorRedeemerFundHoldingDelete, 3)
    , ('ValidatorRedeemerEmergency, 4)
    , ('ValidatorRedeemerFinish, 5)
    , ('ValidatorRedeemerDelete, 6)
    ]

--------------------------------------------------------------------------------2

getValidatorRedeemerName :: Maybe ValidatorRedeemer -> Maybe P.String
getValidatorRedeemerName (Just (ValidatorRedeemerDatumUpdate ValidatorRedeemerDatumUpdateType))             = Just "DatumUpdate"
getValidatorRedeemerName (Just (ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType))           = Just "UpdateMinADA"
getValidatorRedeemerName (Just (ValidatorRedeemerFundHoldingAdd ValidatorRedeemerFundHoldingAddType))       = Just "FundHoldingAdd"
getValidatorRedeemerName (Just (ValidatorRedeemerFundHoldingDelete ValidatorRedeemerFundHoldingDeleteType)) = Just "FundHoldingDelete"
getValidatorRedeemerName (Just (ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType))                 = Just "Emergency"
getValidatorRedeemerName (Just (ValidatorRedeemerFinish ValidatorRedeemerFinishType {}))                    = Just "Finish"
getValidatorRedeemerName (Just (ValidatorRedeemerDelete ValidatorRedeemerDeleteType))                       = Just "Delete"
getValidatorRedeemerName _                                                                                  = Nothing

--------------------------------------------------------------------------------2

mkMintFTRedeemer :: LedgerApiV2.Redeemer
mkMintFTRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            PolicyRedeemerMintFT PolicyRedeemerMintFTType

mkBurnFTRedeemer :: LedgerApiV2.Redeemer
mkBurnFTRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            PolicyRedeemerBurnFT PolicyRedeemerBurnFTType

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

mkFundHoldingAddRedeemer :: LedgerApiV2.Redeemer
mkFundHoldingAddRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerFundHoldingAdd ValidatorRedeemerFundHoldingAddType

mkFundHoldingDeleteRedeemer :: LedgerApiV2.Redeemer
mkFundHoldingDeleteRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerFundHoldingDelete ValidatorRedeemerFundHoldingDeleteType

mkEmergencyRedeemer :: LedgerApiV2.Redeemer
mkEmergencyRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType

mkFinishRedeemer :: LedgerApiV2.POSIXTime -> LedgerApiV2.Redeemer
mkFinishRedeemer date' =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerFinish $ ValidatorRedeemerFinishType date'


mkDeleteRedeemer :: LedgerApiV2.Redeemer
mkDeleteRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerDelete ValidatorRedeemerDeleteType

--------------------------------------------------------------------------------2
