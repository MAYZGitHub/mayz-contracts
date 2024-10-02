--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module TestUtils.Automatic.Helpers where

--------------------------------------------------------------------------------

-- Non-IOG imports

import qualified Data.List.Split           as DataList (splitOn)
import           Prelude                   as P hiding ((<>))

-- IOG imports

-- Project imports
import qualified Data.Char                 as DataChar
import           TestUtils.Automatic.Types

----------------------------------------------------------------------------------------

indent :: Int -> String
indent n = replicate (n * 2) '|'

isWildcard :: String -> Bool
isWildcard s = '*' `elem` s

-- Helper function to get the first element of a triple
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

-- Helper function to get the third element of a tuple
trd :: (a, b, c) -> c
trd (_, _, c) = c

-- Helper function to get the second element of a tuple
snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

-- Helper function to get the fourth element of a tuple
fth4 :: (a, b, c, d) -> d
fth4 (_, _, _, d) = d

-- Helper function to get the second element of a tuple
snd4 :: (a, b, c, d) -> b
snd4 (_, b, _, _) = b

-- Helper function to split the path string into a list of strings
splitPath :: String -> [String]
splitPath = DataList.splitOn "."

safeZipWith :: String -> (a -> b -> c) -> [a] -> [b] -> [c]
safeZipWith name f xs ys
    | length xs /= length ys = P.error $ "Mismatched list lengths in " ++ name ++ ": " ++ show (length xs) ++ " vs " ++ show (length ys)
    | otherwise = zipWith f xs ys


capitalizeFirst :: String -> String
capitalizeFirst ""     = ""  -- Handle the empty string case
capitalizeFirst (x:xs) = DataChar.toUpper x : xs

----------------------------------------------------------------------------------------

convertInputOptionsToTxOutOptions :: InputOptions -> TxOutOptions
convertInputOptionsToTxOutOptions inputOptions' =
    case inputOptions' of
        InputValid -> TxOutValid
        InputInvalid inputInvalidOptions ->
                case inputInvalidOptions of
                    InputInvalidTxOut inputInvalidTxOutOptions -> TxOutInvalid inputInvalidTxOutOptions
                    _                                          -> TxOutValid

--------------------------------------------------------------------------------

