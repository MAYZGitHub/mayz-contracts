{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module TestUtils.Automatic.ParamsGenerators where

--------------------------------------------------------------------------------

-- Non-IOG imports

import qualified Data.Data                             as Data
import qualified Data.Map                              as DataMap
import           Prelude                               as P hiding ((<>))
import qualified Test.QuickCheck                       as QC

-- IOG imports
import qualified Plutus.V2.Ledger.Api                  as LedgerApiV2

-- Project imports

import           TestUtils.Automatic.Types
import           TestUtils.QuickCheckGen.QuickCheckGen
import           TestUtils.Types

----------------------------------------------------------------------------------------

genTxParams :: TxParamGeneratorsList -> String -> QC.Gen [TxParam]
genTxParams namedParams identifier =
    case lookup identifier namedParams of
        Just params -> genParamsWithDependencies params
        Nothing     -> error $ "No TxParamGenerators found for identifier: " ++ identifier

genParamsWithDependencies :: TxParamGenerators -> QC.Gen [TxParam]
genParamsWithDependencies (TxParamGenerators params) = go params DataMap.empty
  where
    go [] _ = return []
    go (TxParamGenerator gen name : rest) accum = do
      value <- gen accum
      let newParam = TxParam name value
      let newAccum = DataMap.insert name newParam accum
      rest' <- go rest newAccum
      return (newParam : rest')


-----------------------------

-- Examples:

-- | Example A simple integer parameter with no dependencies
exampleIntParam :: TxParamGenerator
exampleIntParam = TxParamGenerator
    (\_ -> QC.choose (1 :: Int, 100 :: Int))  -- Generator function: always generates a number between 1 and 100
    "simpleInt"                               -- Parameter name

-- | Example A string parameter with no dependencies
exampleStringParam :: TxParamGenerator
exampleStringParam = TxParamGenerator
    (\_ -> QC.elements (["foo", "bar", "baz"]:: [String]))  -- Generator function: randomly chooses one of these strings
    "exampleString"                            -- Parameter name

exampleDependentParam :: TxParamGenerator
exampleDependentParam = TxParamGenerator
    (\accum -> do
        case DataMap.lookup "simpleInt" accum of
            Just (TxParam _ value) ->
                case Data.cast value of
                    Just (baseValue :: Int) -> QC.choose (baseValue, baseValue * 2)
                    Nothing                 -> error "Expected Int for 'simpleInt', but got a different type"
            Nothing -> error "Required parameter 'simpleInt' not found"
    )
    "dependentInt"  -- Parameter name

-- Usage example with the helper function:
exampleHelperUsage :: TxParamGenerator
exampleHelperUsage = txParamDetail
    (\_ -> QC.vectorOf 5 (QC.choose (1::Integer, 10)))  -- Generator function: creates a list of 5 random numbers between 1 and 10
    "randomList"                               -- Parameter name

-- | Example Using type annotations to avoid ambiguity
exampleWithTypeAnnotation :: TxParamGenerator
exampleWithTypeAnnotation = (txParamDetail :: (DataMap.Map String TxParam -> QC.Gen Double) -> String -> TxParamGenerator)
    (\_ -> QC.choose (0.0, 1.0))  -- Generator function: generates a Double between 0.0 and 1.0
    "probability"                 -- Parameter name

-----------------------------

-- Helpers functions to set the differents parameters

getIntParam :: String -> (Integer -> Bool) -> TxParamGenerator
getIntParam name pred' = TxParamGenerator (\_ -> QC.arbitrary `QC.suchThat` pred') name

positiveIntParam  :: String -> TxParamGenerator
positiveIntParam name = getIntParam name (> 0)

negativeIntParam  :: String ->  TxParamGenerator
negativeIntParam name = getIntParam name (< 0)

intRangeParam :: String -> Integer -> Integer -> TxParamGenerator
intRangeParam name min' max' = TxParamGenerator
                                        (\_ -> QC.choose (min', max'))  -- Generator ignores previous params
                                        name

hexString56Param :: String -> TxParamGenerator
hexString56Param = TxParamGenerator (const genHexString56)

-- Generator for POSIXTime within a specified range
posixTimeRangeParam :: String -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> TxParamGenerator
posixTimeRangeParam name from' to' = TxParamGenerator
    (\_ -> do
        milliseconds <- QC.choose (LedgerApiV2.getPOSIXTime from', LedgerApiV2.getPOSIXTime to')
        return $ LedgerApiV2.POSIXTime milliseconds
    )
    name

-- Generator for POSIXTime that depends on another POSIXTime parameter
dependentAfterPosixTimeRangeParam :: String -> LedgerApiV2.POSIXTime -> String -> TxParamGenerator
dependentAfterPosixTimeRangeParam name = dependentAfterPlusPosixTimeRangeParam name 0

-- Generator for POSIXTime that depends on another POSIXTime parameter
dependentAfterPlusPosixTimeRangeParam :: String -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> String -> TxParamGenerator
dependentAfterPlusPosixTimeRangeParam name afterPlus' maxDistance' dependsOnParamName = TxParamGenerator
    (\accum -> do
        case DataMap.lookup dependsOnParamName accum of
            Just (TxParam _ value) ->
                case Data.cast value of
                    Just (baseTime :: LedgerApiV2.POSIXTime) -> do
                        let minMillis = LedgerApiV2.getPOSIXTime baseTime + LedgerApiV2.getPOSIXTime afterPlus'
                        let maxMillis = minMillis + LedgerApiV2.getPOSIXTime maxDistance'
                        milliseconds <- QC.choose (minMillis, maxMillis)
                        return $ LedgerApiV2.POSIXTime milliseconds
                    Nothing -> error $ "Expected POSIXTime for '" ++ dependsOnParamName ++ "', but got a different type"
            Nothing -> error $ "Required parameter '" ++ dependsOnParamName ++ "' not found"
    )
    name

-- Generator for POSIXTime that depends on another POSIXTime parameter
dependentBeforePosixTimeRangeParam :: String -> LedgerApiV2.POSIXTime -> String -> TxParamGenerator
dependentBeforePosixTimeRangeParam name maxDistance' dependsOnParamName = TxParamGenerator
    (\accum -> do
        case DataMap.lookup dependsOnParamName accum of
            Just (TxParam _ value) ->
                case Data.cast value of
                    Just (baseTime :: LedgerApiV2.POSIXTime) -> do
                        let maxMillis = LedgerApiV2.getPOSIXTime baseTime
                        let minMillis = maxMillis - LedgerApiV2.getPOSIXTime maxDistance'
                        milliseconds <- QC.choose (minMillis, maxMillis)
                        return $ LedgerApiV2.POSIXTime milliseconds
                    Nothing -> error $ "Expected POSIXTime for '" ++ dependsOnParamName ++ "', but got a different type"
            Nothing -> error $ "Required parameter '" ++ dependsOnParamName ++ "' not found"
    )
    name

-- Generator for POSIXTime that depends on two other POSIXTime parameters
dependentBetweenPosixTimeRangeParam :: String -> String -> String -> TxParamGenerator
dependentBetweenPosixTimeRangeParam name = dependentBetweenPlusPosixTimeRangeParam name 0

-- Generator for POSIXTime that depends on two other POSIXTime parameters
dependentBetweenPlusPosixTimeRangeParam :: String -> LedgerApiV2.POSIXTime -> String -> String -> TxParamGenerator
dependentBetweenPlusPosixTimeRangeParam name margin dependsOnParamNameFrom dependsOnParamNameTo = TxParamGenerator
    (\accum -> do
        let lookupTime paramName = case DataMap.lookup paramName accum of
                Just (TxParam _ value) ->
                    case Data.cast value of
                        Just (time :: LedgerApiV2.POSIXTime) -> LedgerApiV2.getPOSIXTime time
                        Nothing                              -> error $ "Expected POSIXTime for '" ++ paramName ++ "', but got a different type"
                Nothing -> error $ "Required parameter '" ++ paramName ++ "' not found"

        let fromMillis = lookupTime dependsOnParamNameFrom + LedgerApiV2.getPOSIXTime margin
        let toMillis = lookupTime dependsOnParamNameTo - LedgerApiV2.getPOSIXTime margin

        if fromMillis >= toMillis
            then error $ "Invalid time range: '" ++ dependsOnParamNameFrom ++ "': "++ show fromMillis ++ ", must be earlier than '" ++ dependsOnParamNameTo ++ "': "++ show fromMillis ++ ""
            else do
                milliseconds <- QC.choose (fromMillis, toMillis)
                return $ LedgerApiV2.POSIXTime milliseconds
    )
    name

-----------------------------

