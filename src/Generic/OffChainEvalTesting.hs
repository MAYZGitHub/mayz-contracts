{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module Generic.OffChainEvalTesting where

--------------------------------------------------------------------------------2
-- External Imports
--------------------------------------------------------------------------------2

import qualified Data.Text                                       as DataText
import qualified Ledger.Ada                                      as LedgerAda
import qualified Ledger.Value                                    as LedgerValue
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api                            as LedgerApiV2
import qualified PlutusCore                                      as PLC
import           PlutusCore.Default                              (DefaultFun, DefaultUni)
import qualified PlutusCore.Evaluation.Machine.ExBudget          as PLC
import           PlutusTx                                        
import qualified PlutusTx.AssocMap                               as TxAssocMap
import qualified PlutusTx.Code                                   as PlutusTxCode
import           PlutusTx.Prelude                                hiding (unless)
import qualified UntypedPlutusCore                               as UPLC
import qualified UntypedPlutusCore.Evaluation.Machine.Cek        as UPLC
import qualified Plutus.V1.Ledger.Interval                       as LedgerIntervalV1

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.DeployHelpers                           as DeployHelpers
import qualified Generic.OnChainHelpers                          as OnChainHelpers
import qualified Protocol.Fund.Helpers                           as FundHelpers
import qualified Protocol.Types                                  as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

-- plcProgram :: PlutusTxCode.CompiledCode a -> UPLC.Program UPLC.NamedDeBruijn DefaultUni  DefaultFun ()
-- plcProgram = PlutusTxCode.getPlc

-- evaluatePlcProgramWithCek :: UPLC.Term UPLC.NamedDeBruijn DefaultUni DefaultFun () -> UPLC.EvaluationResult (UPLC.Term UPLC.NamedDeBruijn DefaultUni DefaultFun ())
-- evaluatePlcProgramWithCek = UPLC.unsafeExtractEvaluationResult . (\(fstT,_,_) -> fstT) . UPLC.runCekDeBruijn PLC.defaultCekParameters UPLC.restrictingEnormous UPLC.noEmitter

-- evaluateCompileCodeWithCekGetCost :: PlutusTxCode.CompiledCode a -> UPLC.RestrictingSt
-- evaluateCompileCodeWithCekGetCost =  (\(_,cost,_) -> cost) . UPLC.runCekDeBruijn PLC.defaultCekParameters UPLC.restrictingEnormous UPLC.noEmitter . plcProgramTerm

plcProgramTerm :: PlutusTxCode.CompiledCode a -> UPLC.Term UPLC.NamedDeBruijn DefaultUni  DefaultFun ()
plcProgramTerm = UPLC._progTerm . PlutusTxCode.getPlc

evaluateCompileCodeWithCek :: PlutusTxCode.CompiledCode a -> UPLC.EvaluationResult (UPLC.Term UPLC.NamedDeBruijn DefaultUni DefaultFun ())
evaluateCompileCodeWithCek = UPLC.unsafeExtractEvaluationResult . (\(fstT,_,_) -> fstT) . UPLC.runCekDeBruijn PLC.defaultCekParameters UPLC.restrictingEnormous UPLC.logEmitter . plcProgramTerm

evaluateCompileCodeWithCekGetCost :: PlutusTxCode.CompiledCode a -> (PLC.ExBudget, [DataText.Text])
evaluateCompileCodeWithCekGetCost code  =
    let (result, UPLC.TallyingSt _ budget, logOut) = UPLC.runCekDeBruijn PLC.defaultCekParameters UPLC.tallying UPLC.logEmitter  (plcProgramTerm code)
    in case result of
            Right _ -> (budget, logOut)
            Left _  -> (budget, logOut)

--------------------------------------------------------------------------------2

{-# INLINEABLE testFunc #-}
testFunc:: Bool
testFunc =
    a + b == 10
    where
        a = 4 ::Integer
        b = 6

testFuncCode :: PlutusTxCode.CompiledCode Bool
testFuncCode = $$( PlutusTx.compile [|| testFunc ||])

--------------------------------------------------------------------------------2


calculateDepositCommissionsUsingMonthsWrapper :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData ->  (Integer, Integer, Integer)
calculateDepositCommissionsUsingMonthsWrapper commissionPerYearInBPx1e3 deadline date deposit  = do
    let
        !commissionPerYearInBPx1e3' =  PlutusTx.unsafeFromBuiltinData commissionPerYearInBPx1e3
        !deadline' =  PlutusTx.unsafeFromBuiltinData deadline
        !date' =  PlutusTx.unsafeFromBuiltinData date
        !deposit' =  PlutusTx.unsafeFromBuiltinData deposit
        !res = FundHelpers.calculateDepositCommissionsUsingMonths commissionPerYearInBPx1e3' deadline' date' deposit'
    res



calculateWithdrawCommissionsUsingMonthsWrapper :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData ->  (Integer, Integer, Integer)
calculateWithdrawCommissionsUsingMonthsWrapper commissionPerYearInBPx1e3 deadline date withdraw  = do
    let
        !commissionPerYearInBPx1e3' =  PlutusTx.unsafeFromBuiltinData commissionPerYearInBPx1e3
        !deadline' =  PlutusTx.unsafeFromBuiltinData deadline
        !date' =  PlutusTx.unsafeFromBuiltinData date
        !withdraw' =  PlutusTx.unsafeFromBuiltinData withdraw
        !investUnit_Granularity = 1
        !res = FundHelpers.calculateWithdrawCommissionsUsingMonths commissionPerYearInBPx1e3' deadline' date' withdraw' investUnit_Granularity
    res


calculateDepositCommissionsUsingMonthsBuiltinDataCodeOptimized :: Integer -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> PlutusTxCode.CompiledCode (Integer, Integer, Integer)
calculateDepositCommissionsUsingMonthsBuiltinDataCodeOptimized commissionPerYearInBPx1e3 deadline date deposit =
    Plutonomy.optimizeUPLC $ calculateDepositCommissionsUsingMonthsBuiltinDataCode commissionPerYearInBPx1e3 deadline date deposit


calculateDepositCommissionsUsingMonthsBuiltinDataCode ::Integer -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> PlutusTxCode.CompiledCode (Integer, Integer, Integer)
calculateDepositCommissionsUsingMonthsBuiltinDataCode commissionPerYearInBPx1e3 deadline date deposit =
    $$(PlutusTx.compile [|| calculateDepositCommissionsUsingMonthsWrapper ||])
            `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData commissionPerYearInBPx1e3)
            `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData $ LedgerApiV2.getPOSIXTime deadline)
            `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData $ LedgerApiV2.getPOSIXTime date)
            `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData deposit)

calculateWithdrawCommissionsUsingMonthsBuiltinDataCode ::Integer -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> PlutusTxCode.CompiledCode (Integer, Integer, Integer)
calculateWithdrawCommissionsUsingMonthsBuiltinDataCode commissionPerYearInBPx1e3 deadline date withdraw =
    $$(PlutusTx.compile [|| calculateWithdrawCommissionsUsingMonthsWrapper ||])
            `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData commissionPerYearInBPx1e3)
            `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData $ LedgerApiV2.getPOSIXTime deadline)
            `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData $ LedgerApiV2.getPOSIXTime date)
            `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData withdraw)


--------------------------------------------------------------------------------

-- {-# INLINEABLE powRational #-}
-- powRational :: Rational -> Integer -> Rational
-- powRational !x !n
--     | n == 0  =  TxRatio.fromInteger  1
--     | x == TxRatio.fromInteger 0  =  TxRatio.fromInteger  0
--     | even n =
--         let !pow' = powRational x (divide n  2)
--         in pow' * pow'
--     | otherwise =
--         let !pow' = powRational x (divide (n - 1)  2)
--         in x * pow' * pow'

-- powRational' :: Integer -> Integer -> Integer -> Rational
-- powRational' !num !dem !n
--     | n == 0  =  TxRatio.fromInteger  1
--     | rati == TxRatio.fromInteger 0  =  TxRatio.fromInteger  0
--     | even n =
--         let !pow' = powRational rati (divide n  2)
--         in pow' * pow'
--     | otherwise =
--         let !pow' = powRational rati (divide (n - 1)  2)
--         in rati * pow' * pow'
--     where
--         rati = TxRatio.unsafeRatio num dem

-- {-# INLINEABLE powRational2' #-}
-- powRational2' :: Integer -> Integer -> Integer -> Rational
-- powRational2' !num !dem !n
--     | n == 0 = TxRatio.fromInteger 1
--     | num == 0  =  TxRatio.fromInteger  0
--     | n < 0 = TxRatio.recip $ powRational2' num dem (negate n)
--     | even n =
--         let !r = powRational2' num dem (n `divide` 2)
--         in r * r
--     | otherwise =
--         let !r = TxRatio.unsafeRatio num dem
--         in r * powRational2' num dem (n - 1)

-- powInteger :: Integer -> Integer -> Integer
-- powInteger num n
--     | n == 0    = 1
--     | num == 0    = 0
--     | even n    = powInteger (num * num) (n `divide` 2)
--     | otherwise = num * powInteger (num * num) (n `divide` 2)

-- powRational3' :: Integer -> Integer -> Integer -> Rational
-- powRational3' !num !dem !n
--     | n == 0    = TxRatio.fromInteger 1
--     | num == 0  = TxRatio.fromInteger 0
--     | n < 0     = TxRatio.recip $ powRational3' num dem (negate n)
--     | otherwise = TxRatio.unsafeRatio (num `powInteger` n) (dem `powInteger` n)

-- reducePrecision1e6 :: Integer -> Integer -> Rational
-- reducePrecision1e6 num den  =
--     let scaleFactor = 1_000_000
--         newNum = num `divide` scaleFactor
--         newDen = den `divide` scaleFactor
--     in TxRatio.unsafeRatio newNum newDen

-- {-# INLINEABLE powInteger #-}
-- powInteger :: Integer -> Integer -> Integer
-- powInteger num n
--     | n == 0    = 1
--     | num == 0    = 0
--     | even n    = powInteger (num * num) (n `divide` 2)
--     | otherwise = num * powInteger (num * num) (n `divide` 2)

-- {-# INLINEABLE powRational #-}
-- powRational :: Integer -> Integer -> Integer -> Rational
-- powRational !num !dem !n
--     | n == 0    = TxRatio.fromInteger 1
--     | num == 0  = TxRatio.fromInteger 0
--     | n < 0     = TxRatio.recip $ powRational num dem (negate n)
--     | otherwise =
--        TxRatio.unsafeRatio  (num `powInteger` n) (dem `powInteger` n)


powRationalWrapper :: BuiltinData -> BuiltinData ->  BuiltinData -> Rational
powRationalWrapper !num !dem !pot  = do
    let
        !num' =  PlutusTx.unsafeFromBuiltinData num
        !dem' =  PlutusTx.unsafeFromBuiltinData dem
        !pot' =  PlutusTx.unsafeFromBuiltinData pot
        !res = OnChainHelpers.powRational num' dem' pot'
    res

-- powRationalWrapper2 :: BuiltinData -> BuiltinData ->  BuiltinData -> Rational
-- powRationalWrapper2 !num !dem !pot  = do
--     let
--         !num' =  PlutusTx.unsafeFromBuiltinData num
--         !dem' =  PlutusTx.unsafeFromBuiltinData dem
--         !pot' =  PlutusTx.unsafeFromBuiltinData pot
--         !res = powRational2' num' dem' pot'
--     res

-- powRationalWrapper3 :: BuiltinData -> BuiltinData ->  BuiltinData -> Rational
-- powRationalWrapper3 !num !dem !pot  = do
--     let
--         !num' =  PlutusTx.unsafeFromBuiltinData num
--         !dem' =  PlutusTx.unsafeFromBuiltinData dem
--         !pot' =  PlutusTx.unsafeFromBuiltinData pot
--         !res = powRational3' num' dem' pot'
--     res

powRationalWrapperBuiltinDataCodeOptimized :: Integer -> Integer -> Integer -> PlutusTxCode.CompiledCode Rational
powRationalWrapperBuiltinDataCodeOptimized num dem pot =
    Plutonomy.optimizeUPLC $ powRationalWrapperBuiltinDataCode num dem pot

powRationalWrapperBuiltinDataCode :: Integer -> Integer -> Integer -> PlutusTxCode.CompiledCode Rational
powRationalWrapperBuiltinDataCode num dem pot =
    Plutonomy.optimizeUPLC $    $$(PlutusTx.compile [|| powRationalWrapper ||])
        `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData num)
        `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData dem)
        `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData pot)

-- powRationalWrapperBuiltinDataCode2 ::Integer -> Integer -> Integer -> PlutusTxCode.CompiledCode Rational
-- powRationalWrapperBuiltinDataCode2 num dem pot =
--             $$(PlutusTx.compile [|| powRationalWrapper2 ||])
--                     `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData num)
--                     `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData dem)
--                     `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData pot)


-- powRationalWrapperBuiltinDataCode3 ::Integer -> Integer -> Integer -> PlutusTxCode.CompiledCode Rational
-- powRationalWrapperBuiltinDataCode3 num dem pot =
--             $$(PlutusTx.compile [|| powRationalWrapper3 ||])
--                     `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData num)
--                     `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData dem)
--                     `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData pot)


--------------------------------------------------------------------------------

-- {-# INLINEABLE setAndLoosePrecision'Old #-}
-- setAndLoosePrecision'Old :: Rational -> Integer -> Rational
-- setAndLoosePrecision'Old !r !n =
--     let num = (10 `powInteger` n)
--     in TxRatio.unsafeRatio (TxRatio.truncate (r * TxRatio.fromInteger  (10 `powInteger` n))) num  --fromInteger (round (r * (10 `pow` n))) / (10.0 ^^ n)

-- setAndLoosePrecision2 :: Rational -> Integer -> Rational
-- setAndLoosePrecision2 !r !n =
--     let !num = 10 `powInteger` n
--         !newNum = TxRatio.truncate (r * TxRatio.fromInteger num)
--     in TxRatio.unsafeRatio newNum num

-- setAndLoosePrecision :: Rational -> Integer -> Rational
-- setAndLoosePrecision !r !n
--     | TxRatio.denominator r < scaleFactor = r
--     | otherwise =
--         let
--             !scaled = r * scaleFactorRational
--             !rounded = TxRatio.truncate scaled
--         in TxRatio.unsafeRatio rounded scaleFactor
--     where
--         !scaleFactor = 10 `powInteger` n
--         !scaleFactorRational = TxRatio.fromInteger scaleFactor
--------------------------------------------------------------------------------


-- ESTO ES LO QUE QUIERO OPTIMIZAR

testDepositWrapper :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData ->  Bool
testDepositWrapper commissionPerYearInBPx1e3 deadline date deposit  fundFT_AC valueOf_FundHoldingDatum_In valueOf_FundHoldingDatum_Out investUnit  = do
    let
        !commissionPerYearInBPx1e3' =  PlutusTx.unsafeFromBuiltinData commissionPerYearInBPx1e3
        !deadline' =  PlutusTx.unsafeFromBuiltinData deadline
        !date' =  PlutusTx.unsafeFromBuiltinData date
        !deposit' =  PlutusTx.unsafeFromBuiltinData deposit
        !fundFT_AC' =  PlutusTx.unsafeFromBuiltinData fundFT_AC
        !valueOf_FundHoldingDatum_In' =  PlutusTx.unsafeFromBuiltinData valueOf_FundHoldingDatum_In
        !valueOf_FundHoldingDatum_Out' =  PlutusTx.unsafeFromBuiltinData valueOf_FundHoldingDatum_Out
        !investUnit' =  PlutusTx.unsafeFromBuiltinData investUnit
        !res = testDeposit commissionPerYearInBPx1e3' deadline' date' deposit' fundFT_AC' valueOf_FundHoldingDatum_In' valueOf_FundHoldingDatum_Out' investUnit'
    res

-- valueOf_FundHoldingDatum_In valueOf_FundHoldingDatum_Out investUnit
-- -> LedgerValue.Value -> LedgerValue.Value -> T.InvestUnit ->

-- instance FromBuiltin LedgerApiV2.AssetClass Bool where
--     {-# INLINABLE fromBuiltin #-}
--     fromBuiltin b = ifThenElse b True False
-- instance ToBuiltin Bool BuiltinBool where
--     {-# INLINABLE toBuiltin #-}
--     toBuiltin b = if b then true else false

-- currencySymbolToBuiltinData :: LedgerApiV2.CurrencySymbol -> BuiltinData
-- currencySymbolToBuiltinData = TxBuiltins.mkB . LedgerApiV2.unCurrencySymbol

-- tokenNameToBuiltinData :: LedgerApiV2.TokenName -> BuiltinData
-- tokenNameToBuiltinData = TxBuiltins.mkB . LedgerApiV2.unTokenName

-- assetClassToBuiltinData :: LedgerValue.AssetClass -> BuiltinData
-- assetClassToBuiltinData ac =
--     let
--         (cs, tn) = LedgerValue.unAssetClass ac
--         !cs' = currencySymbolToBuiltinData cs
--         !tn' = tokenNameToBuiltinData tn
--     in TxBuiltins.mkList  [cs', tn' ]

-- valueToBuiltinData :: LedgerValue.Value -> BuiltinData
-- valueToBuiltinData v  =
--     LedgerApiV2.toBuiltinData  v


testDepositBuiltinDataCodeOptimized :: Integer -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> LedgerValue.AssetClass -> LedgerValue.Value -> LedgerValue.Value ->  T.InvestUnit -> PlutusTxCode.CompiledCode Bool
testDepositBuiltinDataCodeOptimized commissionPerYearInBPx1e3 deadline date deposit fundFT_AC valueOf_FundHoldingDatum_In valueOf_FundHoldingDatum_Out investUnit =
    Plutonomy.optimizeUPLC $ testDepositBuiltinDataCode commissionPerYearInBPx1e3 deadline date deposit fundFT_AC valueOf_FundHoldingDatum_In valueOf_FundHoldingDatum_Out investUnit

testDepositBuiltinDataCode :: Integer -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> LedgerValue.AssetClass -> LedgerValue.Value -> LedgerValue.Value ->  T.InvestUnit -> PlutusTxCode.CompiledCode Bool
testDepositBuiltinDataCode commissionPerYearInBPx1e3 deadline date deposit fundFT_AC valueOf_FundHoldingDatum_In valueOf_FundHoldingDatum_Out investUnit =
           $$(PlutusTx.compile [|| testDepositWrapper ||])
                    -- `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData commissionPerYearInBPx1e3)
                    -- `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData $ LedgerApiV2.getPOSIXTime deadline)
                    -- `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData $ LedgerApiV2.getPOSIXTime date)
                    -- `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData deposit)
                    `PlutusTx.applyCode` PlutusTx.liftCode (LedgerApiV2.toBuiltinData commissionPerYearInBPx1e3)
                    `PlutusTx.applyCode` PlutusTx.liftCode (LedgerApiV2.toBuiltinData deadline)
                    `PlutusTx.applyCode` PlutusTx.liftCode (LedgerApiV2.toBuiltinData date)
                    `PlutusTx.applyCode` PlutusTx.liftCode (LedgerApiV2.toBuiltinData deposit)
                    `PlutusTx.applyCode` PlutusTx.liftCode (LedgerApiV2.toBuiltinData fundFT_AC)
                    `PlutusTx.applyCode` PlutusTx.liftCode (LedgerApiV2.toBuiltinData valueOf_FundHoldingDatum_In)
                    `PlutusTx.applyCode` PlutusTx.liftCode (LedgerApiV2.toBuiltinData valueOf_FundHoldingDatum_Out)
                    `PlutusTx.applyCode` PlutusTx.liftCode (LedgerApiV2.toBuiltinData investUnit)

fundFT_TN_ :: LedgerApiV2.TokenName
fundFT_TN_ = LedgerApiV2.TokenName "FT"

fundFT_AC_ :: LedgerValue.AssetClass
fundFT_AC_ = LedgerValue.AssetClass ("d158327544bca825cdabd07f96727f64f5262fcfbf661c3b5f9118cb",  fundFT_TN_)

valueOf_FundHoldingDatum_In_ :: LedgerValue.Value
valueOf_FundHoldingDatum_In_ = LedgerAda.lovelaceValueOf 12241330 <> LedgerApiV2.singleton "74854c7cd622e151aeef59b7d97fe0d60e8e69a10adbe13c19e918aa" "FundHoldingID0" 1

-- (,\"\",12241330)"
-- (6aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759,\"token1\",1000000)
-- (74854c7cd622e151aeef59b7d97fe0d60e8e69a10adbe13c19e918aa,\"FundHoldingID0\",1)
-- (d158327544bca825cdabd07f96727f64f5262fcfbf661c3b5f9118cb,\"FT\",24703)


valueOf_FundHoldingDatum_Out_ :: LedgerValue.Value
valueOf_FundHoldingDatum_Out_ = LedgerAda.lovelaceValueOf 12241330  <> LedgerApiV2.singleton "74854c7cd622e151aeef59b7d97fe0d60e8e69a10adbe13c19e918aa" "FundHoldingID0" 1 <>  LedgerApiV2.singleton "6aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759" "token" 1000000 <>  LedgerApiV2.singleton "d158327544bca825cdabd07f96727f64f5262fcfbf661c3b5f9118cb"  fundFT_TN_ 12923

token1_ :: T.InvestUnitToken
token1_ = ( "6aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759","token", 1) :: T.InvestUnitToken

investUnit_ :: T.InvestUnit
investUnit_ = T.InvestUnit { T.iuValues = [] }

--
--

testDeposit :: [Integer] -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer ->  LedgerValue.AssetClass -> LedgerValue.Value -> LedgerValue.Value -> T.InvestUnit ->   Bool
testDeposit commissionsTable_Numerator1e6 deadline date deposit fundFT_AC valueOf_FundHoldingDatum_In valueOf_FundHoldingDatum_Out investUnit =
    --traceIfFalse "not isCorrect_Output_FundHolding_Datum_With_Deposit" (isCorrect_Output_FundHolding_Datum fundHoldingDatum_Control_With_Deposit)
    -- &&
    traceIfFalse "not isCorrect_Output_FundHolding_Value_With_Tokens_And_FT" (isCorrect_Output_FundHolding_Value valueFor_FundHoldingDatum_Control_With_Tokens_And_FT valueOf_FundHoldingDatum_Out)
    --     &&
    --     traceIfFalse "not isMintingFT" isMintingFT
    --     && traceIfFalse "not isDateInRange" (OnChainHelpers.isDateInRange date info)
    where
    ------------------
        !(_, commissionsFT, _) = FundHelpers.calculateDepositCommissionsUsingMonths commissionsTable_Numerator1e6 deadline date deposit
        -- !(userFT, commissionsFT, commissions_FT_Rate1e6_PerMonth) = (987077,12923,1076916666)
    ------------------
        -- !valueOf_TokensForDeposit = createValue_WithTokensFrom_InvestUnit deposit
        !valueOf_TokensForDeposit_Plus_FundHoldingDatum_Value = createValue_WithTokensFrom_InvestUnit_Plus_FundHoldingDatum_Value valueOf_FundHoldingDatum_In deposit  investUnit
    ------------------
        !valueFor_FT_Commissions = LedgerValue.assetClassValue fundFT_AC commissionsFT
    ------------------
        !valueFor_FundHoldingDatum_Control_With_Tokens_And_FT = valueOf_TokensForDeposit_Plus_FundHoldingDatum_Value <> valueFor_FT_Commissions -- valueOf_FundHoldingDatum_In -- <> valueOf_TokensForDeposit <> valueFor_FT_Commissions
    ------------------
        -- !fundHoldingDatum_Control_With_Deposit = FundHelpers.mkUpdated_FundHolding_Datum_With_Deposit fundHoldingDatum_In deposit userFT commissionsFT commissions_FT_Rate1e6_PerMonth
    ------------------
        -- isMintingFT :: Bool
        -- isMintingFT = FundHelpers.isToken_Minting_With_AC_AndAmt fundFT_AC deposit info

-- isCorrect_Output_FundHolding_Datum :: FundHoldingT.FundHoldingDatumType -> Bool
-- isCorrect_Output_FundHolding_Datum fundHoldingDatum_Control =
--     let !fundHoldingDatum_Out = FundHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_FundHoldingDatum
--     in  fundHoldingDatum_Out `FundHelpers.isUnsafeEqDatums` fundHoldingDatum_Control
------------------

isCorrect_Output_FundHolding_Value :: LedgerValue.Value -> LedgerValue.Value -> Bool
isCorrect_Output_FundHolding_Value valueFor_FundHoldingDatum_Control valueOf_FundHoldingDatum_Out =
    valueOf_FundHoldingDatum_Out `OnChainHelpers.isEqValue` valueFor_FundHoldingDatum_Control

------------------

fixAmounts2 :: Integer -> [(a, b, Integer)] -> [(a, b, Integer)] -> [(a, b, Integer)]
fixAmounts2 _ [] res = res
fixAmounts2 amount ((cs, tn, amt): restTokens) res  =
    let
        !amt' = (amt * amount) `divide` 100
    in fixAmounts2 amount restTokens ((cs, tn, amt'): res)

createValue_WithTokensFrom_InvestUnit_Plus_FundHoldingDatum_Value :: LedgerValue.Value -> Integer -> T.InvestUnit -> LedgerValue.Value
createValue_WithTokensFrom_InvestUnit_Plus_FundHoldingDatum_Value (LedgerValue.Value mp) amount investUnit =
    LedgerValue.Value mapCSResult
    where
        !investUnitTokens = T.iuValues investUnit
        ------------------
        !listMapCS = TxAssocMap.toList mp
        !listTokens =  investUnitTokens
        !mapCSResult = TxAssocMap.fromList (updateListMapCS listTokens listMapCS)
        ------------------
        updateListMapCS [] restListMapCS = restListMapCS
        updateListMapCS ((cs, tn, amt): restTokens) restListMapCS  =
            let
                !(tokensFromSameCS, restTokensWithoutCS) = getOthersTokensFromSameCSAndDeleteFromList cs restTokens [] []
                !(mapFromSameCS, restMapWithoutCS) = getMapFromSameCSAndDeleteFromList cs restListMapCS []
                !mapFromSameCSWithTokensAdded = addTokensInMap cs mapFromSameCS ((tn, amt):tokensFromSameCS)
                !resultMap = mapFromSameCSWithTokensAdded : updateListMapCS restTokensWithoutCS restMapWithoutCS
            in resultMap
        ------------------
        getOthersTokensFromSameCSAndDeleteFromList _ [] accListFromSame accListOthers = (accListFromSame, accListOthers)
        getOthersTokensFromSameCSAndDeleteFromList cs ((cs', tn', amt'): restTokens) accListFromSame accListOthers
            | cs == cs' = getOthersTokensFromSameCSAndDeleteFromList cs restTokens ( (tn', amt'): accListFromSame) accListOthers
            | otherwise = getOthersTokensFromSameCSAndDeleteFromList cs restTokens accListFromSame ( (cs', tn', amt'): accListOthers)
        ------------------
        getMapFromSameCSAndDeleteFromList _ [] accListMapOthers = (Nothing, accListMapOthers)
        getMapFromSameCSAndDeleteFromList cs ((cs', mapTN): restMap) accListOthers
            | cs == cs' = (Just mapTN, restMap ++ accListOthers)
            | otherwise = getMapFromSameCSAndDeleteFromList cs restMap ( (cs', mapTN): accListOthers)
        ------------------
        addTokensInMap cs Nothing tokens      = addTokensInMap' cs TxAssocMap.empty tokens
        addTokensInMap cs (Just mapTN) tokens = addTokensInMap' cs mapTN tokens
        ------------------
        -- amt' in investUnit is multiplied by 100, so the real value comes divided by 100. Im sure this division is exact because i checked before in isCorrectAmount
        addTokensInMap' cs mapTN []                   = (cs, mapTN)
        addTokensInMap' cs mapTN listTokensToAddInMap = (cs, foldl (\acc (tn', amt') -> mapElement acc tn' ((amt' * amount) `divide` 100)) mapTN listTokensToAddInMap)
        ------------------
        mapElement acc tn amt =
            case TxAssocMap.lookup tn acc of
                Nothing   -> TxAssocMap.insert tn amt acc
                Just amt' -> TxAssocMap.insert tn (amt + amt') acc
    ------------------


createValue_WithTokensFrom_InvestUnit_Plus_FundHoldingDatum_Value2 :: LedgerValue.Value -> Integer -> T.InvestUnit -> LedgerValue.Value
createValue_WithTokensFrom_InvestUnit_Plus_FundHoldingDatum_Value2 (LedgerValue.Value mp) amount investUnit =
    LedgerValue.Value mapCSResult
    where
        !listMapCS = TxAssocMap.toList mp
        !investUnitTokens = T.iuValues investUnit
        !listTokensIU =   investUnitTokens
        ------------------
        !mapCSResult = TxAssocMap.fromList (updateListMapCS listTokensIU listMapCS)
        ------------------
        updateListMapCS [] restListMapCS = restListMapCS
        updateListMapCS ((cs, tn, amt): restTokens) restListMapCS  =
            let
                !(tokensFromSameCS, restTokensWithoutCS) = getOthersTokensFromSameCSAndDeleteFromList cs restTokens [] []
                !(mapFromSameCS, restMapWithoutCS) = getMapFromSameCSAndDeleteFromList cs restListMapCS []
                !mapFromSameCSWithTokensAdded = addTokensInMap cs mapFromSameCS ((tn, amt):tokensFromSameCS)
                !resultMap = mapFromSameCSWithTokensAdded : updateListMapCS restTokensWithoutCS restMapWithoutCS
            in resultMap
        ------------------
        getOthersTokensFromSameCSAndDeleteFromList _ [] accListFromSame accListOthers = (accListFromSame, accListOthers)
        getOthersTokensFromSameCSAndDeleteFromList cs ((cs', tn', amt'): restTokens) accListFromSame accListOthers
            | cs == cs' = getOthersTokensFromSameCSAndDeleteFromList cs restTokens ( (tn', amt'): accListFromSame) accListOthers
            | otherwise = getOthersTokensFromSameCSAndDeleteFromList cs restTokens accListFromSame ( (cs', tn', amt'): accListOthers)
        ------------------
        getMapFromSameCSAndDeleteFromList _ [] accListMapOthers = (Nothing, accListMapOthers)
        getMapFromSameCSAndDeleteFromList cs ((cs', mapTN): restMap) accListOthers
            | cs == cs' = (Just mapTN, restMap ++ accListOthers)
            | otherwise = getMapFromSameCSAndDeleteFromList cs restMap ( (cs', mapTN): accListOthers)
        ------------------
        addTokensInMap cs Nothing tokens      = addTokensInMap' cs TxAssocMap.empty tokens
        addTokensInMap cs (Just mapTN) tokens = addTokensInMap' cs mapTN tokens
        ------------------
        -- amt' in investUnit is multiplied by 100, so the real value comes divided by 100. Im sure this division is exact because i checked before in isCorrectAmount
        addTokensInMap' cs mapTN []                   = (cs, mapTN)
        addTokensInMap' cs mapTN listTokensToAddInMap = (cs, foldl (\acc (tn', amt') -> mapElement acc tn' ((amt' * amount) `divide` 100)) mapTN listTokensToAddInMap)
        ------------------
        mapElement acc tn amt =
            case TxAssocMap.lookup tn acc of
                Nothing   -> TxAssocMap.insert tn amt acc
                Just amt' -> TxAssocMap.insert tn (amt + amt') acc


------------------
isFundOpen :: LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> LedgerIntervalV1.Interval LedgerApiV2.POSIXTime-> Bool
isFundOpen fdBeginAt fdDeadline interval = isDateReached fdBeginAt  interval && isDateNotReached fdDeadline interval
------------------

isDateReached :: LedgerApiV2.POSIXTime -> LedgerIntervalV1.Interval LedgerApiV2.POSIXTime-> Bool
isDateReached !date !interval = LedgerIntervalV1.contains (LedgerIntervalV1.from date)  interval

isDateNotReached :: LedgerApiV2.POSIXTime -> LedgerIntervalV1.Interval LedgerApiV2.POSIXTime -> Bool
isDateNotReached !date !interval = not (isDateReached date interval)

------------------------


