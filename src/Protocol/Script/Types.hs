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

module Protocol.Script.Types where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Data.Aeson             as DataAeson
import qualified Data.OpenApi.Schema    as DataOpenApiSchema
import qualified GHC.Generics           as GHCGenerics
import qualified Ledger.Value           as LedgerValue
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
import qualified Protocol.Types         as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

-- This is very important: its the way to specify the kind of datum that the validator expects
-- and has admins inside, so it can check if a proper admin is signing the transaction
-- I wanted to write a generic script validator, that works for protocol Datums and Fund Datum, but I could add more
-- All the types of datum should have the HasAdmin typeclass

-- data DatumWithAdmin = Protocol ProtocolT.ProtocolDatumType  |  Fund FundT.FundDatumType
--     deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

--------------------------------------------------------------------------------2
-- Params
--------------------------------------------------------------------------------2

newtype PolicyParams
    = PolicyParams { ppProtocolPolicyID_CS :: T.CS }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq PolicyParams where
    {-# INLINEABLE (==) #-}
    p1 == p2 = ppProtocolPolicyID_CS p1 == ppProtocolPolicyID_CS p2

PlutusTx.makeLift ''PolicyParams
PlutusTx.makeIsDataIndexed ''PolicyParams [('PolicyParams, 0)]

data ValidatorParams
    = ValidatorParams
          { vpProtocolPolicyID_CS :: T.CS
          , vpScriptPolicyID_CS   :: T.CS
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq ValidatorParams where
    {-# INLINEABLE (==) #-}
    p1 == p2 =
        vpProtocolPolicyID_CS p1 == vpProtocolPolicyID_CS p2  &&
        vpScriptPolicyID_CS p1 == vpScriptPolicyID_CS p2

PlutusTx.makeLift ''ValidatorParams
PlutusTx.makeIsDataIndexed ''ValidatorParams [('ValidatorParams, 0)]

--------------------------------------------------------------------------------2
-- Datums
--------------------------------------------------------------------------------2

data ScriptDatumType
    = ScriptDatumType
          { sdFundPolicy_CS   :: Maybe T.CS
          , sdAdminPaymentPKH :: T.WalletPaymentPKH
          , sdAdminStakePKH   :: Maybe T.WalletPaymentPKH
          , sdScriptHash      :: BuiltinByteString
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq ScriptDatumType where
    {-# INLINEABLE (==) #-}
    sd1 == sd2 =
        -- sdID_CS sd1 == sdID_CS sd2
        sdFundPolicy_CS sd1 == sdFundPolicy_CS sd2
            && sdAdminPaymentPKH sd1 == sdAdminPaymentPKH sd2
            && sdAdminStakePKH sd1 == sdAdminStakePKH sd2
            && sdScriptHash sd1 == sdScriptHash sd2

PlutusTx.makeIsDataIndexed ''ScriptDatumType [('ScriptDatumType, 0)]

newtype ValidatorDatum
    = ScriptDatum ScriptDatumType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq ValidatorDatum where
    {-# INLINEABLE (==) #-}
    ScriptDatum sd1 == ScriptDatum sd2 = sd1 == sd2

PlutusTx.makeIsDataIndexed ''ValidatorDatum [('ScriptDatum, 0)]

{-# INLINEABLE getScript_DatumType #-}
getScript_DatumType :: ValidatorDatum -> ScriptDatumType
getScript_DatumType (ScriptDatum sdType) = sdType

{-# INLINEABLE getScript_DatumType_From_UTxO #-}
getScript_DatumType_From_UTxO :: LedgerApiV2.TxOut -> ScriptDatumType
getScript_DatumType_From_UTxO utxo = case OnChainHelpers.getInlineDatum_From_TxOut @ValidatorDatum utxo of
                    Nothing     -> P.error "No Script Datum found"
                    Just datum' -> getScript_DatumType datum'

instance T.ShowDatum ValidatorDatum where
    showCborAsDatumType cbor = case LedgerApiV2.fromBuiltinData @ValidatorDatum cbor of
        Nothing -> Nothing
        Just d  -> Just $ P.show d

--------------------------------------------------------------------------------2

{-# INLINEABLE mkScriptDatumType #-}
mkScriptDatumType :: Maybe T.CS -> T.WalletPaymentPKH -> Maybe T.WalletPaymentPKH -> BuiltinByteString -> ScriptDatumType
mkScriptDatumType = ScriptDatumType

{-# INLINEABLE mkScriptDatum #-}
mkScriptDatum :: Maybe T.CS -> T.WalletPaymentPKH -> Maybe T.WalletPaymentPKH -> BuiltinByteString -> ValidatorDatum
mkScriptDatum
    fundPolicy_CS
    adminPaymentPKH
    adminStakePKH
    scriptHash =
        ScriptDatum $
            mkScriptDatumType
                fundPolicy_CS
                adminPaymentPKH
                adminStakePKH
                scriptHash

mkDatum :: ScriptDatumType -> LedgerApiV2.Datum
mkDatum = LedgerApiV2.Datum . LedgerApiV2.toBuiltinData . ScriptDatum

--------------------------------------------------------------------------------2
-- PolicyRedeemer
--------------------------------------------------------------------------------2

data PolicyRedeemerMintIDType = PolicyRedeemerMintIDType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerMintIDType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

-- r1 == r2 =   rmAdminPaymentPKH r1 == rmAdminPaymentPKH r2
--     && rmAdminStakePKH r1 == rmAdminStakePKH r2

PlutusTx.unstableMakeIsData ''PolicyRedeemerMintIDType

data PolicyRedeemerBurnIDType = PolicyRedeemerBurnIDType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerBurnIDType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        r1 == r2

-- rbAdminPaymentPKH r1 == rbAdminPaymentPKH r2
-- && rbAdminStakePKH r1 == rbAdminStakePKH r2

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
    [ ('PolicyRedeemerMintID, 1),
      ('PolicyRedeemerBurnID, 2)
    ]

--------------------------------------------------------------------------------2

getPolicyRedeemerName :: Maybe PolicyRedeemer -> Maybe P.String
getPolicyRedeemerName (Just (PolicyRedeemerMintID PolicyRedeemerMintIDType)) = Just "MintID"
getPolicyRedeemerName (Just (PolicyRedeemerBurnID PolicyRedeemerBurnIDType)) = Just "BurnID"
getPolicyRedeemerName _                                                      = Nothing

--------------------------------------------------------------------------------2

--------------------------------------------------------------------------------2
-- ValidatorRedeemer
--------------------------------------------------------------------------------2

data ValidatorRedeemerDelete = ValidatorRedeemerDelete deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerDelete where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerDelete [('ValidatorRedeemerDelete, 0)]

type ValidatorRedeemer = ValidatorRedeemerDelete

getValidatorRedeemerName :: Maybe ValidatorRedeemer -> Maybe P.String
getValidatorRedeemerName (Just ValidatorRedeemerDelete) = Just "Delete"
getValidatorRedeemerName _                              = Nothing

------------------------------------------------------------------------------

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

mkScriptDeleteRedeemer :: LedgerApiV2.Redeemer
mkScriptDeleteRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData
            ValidatorRedeemerDelete

--------------------------------------------------------------------------------2
