{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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

module Protocol.Types where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Data.Aeson as DataAeson (FromJSON, ToJSON)
import qualified Data.OpenApi.Schema as DataOpenApiSchema
import qualified GHC.Generics as GHCGenerics (Generic)
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx
import qualified PlutusTx.Builtins as TxBuiltins
import PlutusTx.Prelude hiding (unless)
import qualified Schema
import qualified Prelude as P

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.Types as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

class HasAdmins a where
    getAdmins :: a -> [T.WalletPaymentPKH]

class HasAdminToken a where
    getAdminToken_CS :: a -> LedgerApiV2.CurrencySymbol

class ShowDatum datum where
    showCborAsDatumType :: BuiltinData -> Maybe P.String

--------------------------------------------------------------------------------2

newtype Estado = Estado {getEstado :: P.String}
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

--------------------------------------------------------------------------------2

-- deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema, Schema.ToSchema)
-- instance P.Ord LedgerApiV2.Value

--------------------------------------------------------------------------------2

type InvestUnitToken = (LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)

newtype InvestUnit = InvestUnit {iuValues :: [InvestUnitToken]}
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Schema.ToSchema InvestUnit where
    toSchema = Schema.FormSchemaUnit

instance Eq InvestUnit where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        TxBuiltins.serialiseData (LedgerApiV2.toBuiltinData r1) == TxBuiltins.serialiseData (LedgerApiV2.toBuiltinData r2)

-- iuValues r1 `OnChainHelpers.isEqFlattenValue` iuValues r2

PlutusTx.makeIsDataIndexed
    ''InvestUnit
    [ ('InvestUnit, 0)
    ]

--------------------------------------------------------------------------------2

data OracleReIdx_Data = OracleReIdx_Data
    { oridTokensPriceADA :: InvestUnit
    , oridTime :: LedgerApiV2.POSIXTime
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq OracleReIdx_Data where
    {-# INLINEABLE (==) #-}
    ps1 == ps2 =
        oridTokensPriceADA ps1 == oridTokensPriceADA ps2
            && oridTime ps1 == oridTime ps2

PlutusTx.makeIsDataIndexed
    ''OracleReIdx_Data
    [ ('OracleReIdx_Data, 0)
    ]

data Oracle_Data = Oracle_Data
    { odFTPriceADA1xe6 :: InvestUnit
    , odTime :: LedgerApiV2.POSIXTime
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq Oracle_Data where
    {-# INLINEABLE (==) #-}
    ps1 == ps2 =
        odFTPriceADA1xe6 ps1 == odFTPriceADA1xe6 ps2
            && odTime ps1 == odTime ps2

PlutusTx.makeIsDataIndexed
    ''Oracle_Data
    [ ('Oracle_Data, 0)
    ]

--------------------------------------------------------------------------------2

