{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
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

module Protocol.InvestUnit.Types where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Data.Aeson           as DataAeson (FromJSON, ToJSON)
import qualified Data.OpenApi.Schema  as DataOpenApiSchema (ToSchema)
import qualified GHC.Generics         as GHCGenerics (Generic)
import qualified Ledger
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude              as P
import qualified Schema

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.Types        as T
import qualified Protocol.Types       as T
import qualified Generic.OnChainHelpers as OnChainHelpers

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

--------------------------------------------------------------------------------2
-- Params
--------------------------------------------------------------------------------2

data ValidatorParams
    = ValidatorParams
          { vpProtocolPolicyID_CS          :: LedgerApiV2.CurrencySymbol
          , vpTokenEmergencyAdminPolicy_CS :: LedgerApiV2.CurrencySymbol
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq ValidatorParams where
    {-# INLINEABLE (==) #-}
    pp1 == pp2 =
        vpProtocolPolicyID_CS pp1 == vpProtocolPolicyID_CS pp2
            && vpTokenEmergencyAdminPolicy_CS pp1 == vpTokenEmergencyAdminPolicy_CS pp2

PlutusTx.makeLift ''ValidatorParams
PlutusTx.makeIsDataIndexed
    ''ValidatorParams
    [ ('ValidatorParams, 0)
    ]

--------------------------------------------------------------------------------2
-- Datums
--------------------------------------------------------------------------------2

data InvestUnitDatumType
    = InvestUnitDatumType
          { iudFundPolicy_CS :: T.CS
          , iudInvestUnit    :: T.InvestUnit
          , iudMinADA        :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq InvestUnitDatumType where
    {-# INLINEABLE (==) #-}
    ps1 == ps2 =
        iudFundPolicy_CS ps1 == iudFundPolicy_CS ps2
            && iudInvestUnit ps1 == iudInvestUnit ps2
            && iudMinADA ps1 == iudMinADA ps2


PlutusTx.makeIsDataIndexed
    ''InvestUnitDatumType
    [ ('InvestUnitDatumType, 0)
    ]

--------------------------------------------------------------------------------2

newtype ValidatorDatum
    = InvestUnitDatum InvestUnitDatumType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq ValidatorDatum where
    {-# INLINEABLE (==) #-}
    InvestUnitDatum mps1 == InvestUnitDatum mps2 = mps1 == mps2

PlutusTx.makeIsDataIndexed
    ''ValidatorDatum
    [ ('InvestUnitDatum, 0)
    ]

{-# INLINEABLE getInvestUnit_DatumType #-}
getInvestUnit_DatumType :: ValidatorDatum -> InvestUnitDatumType
getInvestUnit_DatumType (InvestUnitDatum sdType) = sdType

{-# INLINEABLE getInvestUnit_DatumType_From_UTxO #-}
getInvestUnit_DatumType_From_UTxO :: LedgerApiV2.TxOut -> InvestUnitDatumType
getInvestUnit_DatumType_From_UTxO utxo = case OnChainHelpers.getInlineDatum_From_TxOut @ValidatorDatum utxo of
                    Nothing     -> P.error "No InvestUnit Datum found"
                    Just datum' -> getInvestUnit_DatumType datum'

instance T.ShowDatum ValidatorDatum where
    showCborAsDatumType cbor = case LedgerApiV2.fromBuiltinData @ValidatorDatum cbor of
        Nothing -> Nothing
        Just d  -> Just $ P.show d

--------------------------------------------------------------------------------2

{-# INLINEABLE mkInvestUnit_DatumType #-}
mkInvestUnit_DatumType :: T.CS -> T.InvestUnit -> Integer -> InvestUnitDatumType
mkInvestUnit_DatumType = InvestUnitDatumType

{-# INLINEABLE mkInvestUnit_Datum #-}
mkInvestUnit_Datum :: T.CS -> T.InvestUnit -> Integer -> ValidatorDatum
mkInvestUnit_Datum fundPolicy_CS investUnit minADA =
    InvestUnitDatum $ mkInvestUnit_DatumType fundPolicy_CS investUnit minADA

mkDatum :: InvestUnitDatumType -> LedgerApiV2.Datum
mkDatum = LedgerApiV2.Datum . LedgerApiV2.toBuiltinData . InvestUnitDatum


--------------------------------------------------------------------------------2
-- ValidatorRedeemer
--------------------------------------------------------------------------------2

data ValidatorRedeemerUpdateMinADAType
    = ValidatorRedeemerUpdateMinADAType 
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerUpdateMinADAType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerUpdateMinADAType [('ValidatorRedeemerUpdateMinADAType, 0)]

--------------------------------------------------------------------------------2
data ValidatorRedeemerReIndexingType
    = ValidatorRedeemerReIndexingType
          { riuriTokensToAdd      :: T.InvestUnit
          , riuriTokensToRemove   :: T.InvestUnit
          , riuriOracleReIdx_Data :: T.OracleReIdx_Data
          , riuriOracleSignature  :: Ledger.Signature
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerReIndexingType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        riuriTokensToAdd r1 == riuriTokensToAdd r2
        && riuriTokensToRemove r1 == riuriTokensToRemove r2
        && riuriOracleReIdx_Data r1 == riuriOracleReIdx_Data r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerReIndexingType
    [ ('ValidatorRedeemerReIndexingType, 0)
    ]

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
    = ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType
    | ValidatorRedeemerReIndexing ValidatorRedeemerReIndexingType
    | ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemer where
    {-# INLINEABLE (==) #-}
    ValidatorRedeemerUpdateMinADA rmf1 == ValidatorRedeemerUpdateMinADA rmf2 = rmf1 == rmf2
    ValidatorRedeemerReIndexing rmf1 == ValidatorRedeemerReIndexing rmf2     = rmf1 == rmf2
    ValidatorRedeemerEmergency rmf1 == ValidatorRedeemerEmergency rmf2       = rmf1 == rmf2
    _ == _                                                                   = False

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemer
    [ ('ValidatorRedeemerReIndexing, 0),
        ('ValidatorRedeemerUpdateMinADA, 1),
        ('ValidatorRedeemerEmergency, 2)
    ]

--------------------------------------------------------------------------------2

getValidatorRedeemerName :: Maybe ValidatorRedeemer -> Maybe P.String
getValidatorRedeemerName (Just (ValidatorRedeemerReIndexing ValidatorRedeemerReIndexingType {})) = Just "ReIndexing"
getValidatorRedeemerName (Just (ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType)) = Just "UpdateMinADA"
getValidatorRedeemerName (Just (ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType)) = Just "Emergency"
getValidatorRedeemerName _ = Nothing

--------------------------------------------------------------------------------2

mkUpdateMinADARedeemer :: LedgerApiV2.Redeemer
mkUpdateMinADARedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType

mkReIndexingRedeemer ::
    T.InvestUnit ->
    T.InvestUnit ->
    T.OracleReIdx_Data ->
    Ledger.Signature ->
    LedgerApiV2.Redeemer
mkReIndexingRedeemer tokensToAdd' tokensToRemove' oracle sig =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerReIndexing $
                ValidatorRedeemerReIndexingType
                    tokensToAdd'
                    tokensToRemove'
                    oracle
                    sig

mkEmergencyRedeemer :: LedgerApiV2.Redeemer
mkEmergencyRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType

--------------------------------------------------------------------------------2
