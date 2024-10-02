{-# LANGUAGE GADTs #-}

--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

{- |
Module      : TestUtils.TypesMAYZ
Description : Types for the Test Utils.
-}
module TestUtils.Types where

--------------------------------------------------------------------------------

-- Non-IOG imports

import qualified Data.Data            as Data
import qualified Data.List            as DataList
import qualified Data.Map             as DataMap
import qualified Data.Maybe           as DataMaybe
import           Prelude              as P

-- IOG imports
import qualified Plutus.V2.Ledger.Api as LedgerApiV2

-- Project imports

--------------------------------------------------------------------------------

-- | Pipe operator.
(|>) :: a -> (a -> b) -> b
x |> f = f x

--------------------------------------------------------------------------------

class Pretty a where
    pretty :: a -> String

--------------------------------------------------------------------------------

stripSuffix :: String -> String -> String
stripSuffix suffix str =
    DataMaybe.fromMaybe str $
        if suffix `DataList.isSuffixOf` str
        then Just (take (length str - length suffix) str)
        else Nothing

--------------------------------------------------------------------------------

-- Type-safe wrapper for generated parameters
data TxParam where
    TxParam :: (Data.Typeable a, Show a) => String -> a -> TxParam

instance Show TxParam where
    show (TxParam name x) = "TxParam " ++ name ++ ": " ++ show x

--------------------------------------------

type TxContextParametrizable = [TxParam] -> LedgerApiV2.ScriptContext

--------------------------------------------

getTxParam :: Data.Typeable a => String -> [TxParam] -> a
getTxParam paramName txParams =
    let
        paramMap = DataMap.fromList [(name, param) | param@(TxParam name _) <- txParams]

        extractParam :: Data.Typeable a => String -> Maybe a
        extractParam name = do
            TxParam _ value <- DataMap.lookup name paramMap
            Data.cast value
    in
        case extractParam paramName of
            Just value -> value
            Nothing    -> P.error $ "Failed to extract parameter: " ++ paramName

--------------------------------------------
-- Examples:

exampleExtraction :: [TxParam] -> (Integer, Integer, Integer)
exampleExtraction txParams =
    ( getTxParam "token_FT_Price1xe6" txParams
    , getTxParam "amount_ADA" txParams
    , getTxParam "available_FT" txParams
    )

--------------------------------------------
