{-# LANGUAGE TypeFamilies #-}
--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2
{- |
Module      : InvestUnit.Validator
Description : Validation logic and tests related to the InvestUnit module.

This module defines the validation logic for the InvestUnit contracts.

It includes multiple test cases to ensure the integrity and correctness of the
validation scripts.
-}
module Contracts.InvestUnit.Validator where
--------------------------------------------------------------------------------

-- Non-IOG imports
import           Prelude
import qualified Test.Tasty                               as Tasty

-- IOG imports

-- Project imports
import qualified Generic.Constants                        as T
import qualified Generic.OffChainHelpers                  as OffChainHelpers
import qualified Ledger
import qualified Ledger.Ada                               as LedgerAda
import qualified Ledger.Crypto                            as LedgerCrypto
import qualified Plutus.V2.Ledger.Api                     as LedgerApiV2
import           PlutusTx.Prelude                         (divide)
import qualified Protocol.Constants                       as T
import qualified Protocol.Fund.Holding.Types              as FundHoldingT
import qualified Protocol.InvestUnit.Types                as InvestUnitT
import qualified Protocol.OnChainHelpers                  as OnChainHelpers
import qualified Protocol.Types                           as T
import qualified Test.Tasty.HUnit                         as Tasty
import           TestUtils.Contracts.InitialData
import           TestUtils.Contracts.TxContext.InvestUnit
import           TestUtils.HelpersMAYZ
import           TestUtils.TestContext.Asserts
import           TestUtils.TestContext.Helpers
import           TestUtils.Types
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------

investUnit_Validator_Tests :: TestParams -> Tasty.TestTree
investUnit_Validator_Tests tp =
    Tasty.testGroup
        "InvestUnit Validator Tests"
        [
            investUnit_Validator_Redeemer_ReIndexing_Tests tp
            , investUnit_Validator_Redeemer_UpdateMinADA_Tests tp
        ]

--------------------------------------------------------------------------------

investUnit_Validator_Redeemer_ReIndexing_Tests :: TestParams -> Tasty.TestTree
investUnit_Validator_Redeemer_ReIndexing_Tests tp =
    let
        ------------------------
        txName = show Fund_ReIndexing_Tx
        selectedRedeemer = RedeemerLogValidator (Just InvestUnit_ReIndexing_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = investUnit_ReIndexing_TxContext tp
                in
                    [
                        Tasty.testCase "ReIndexing correctly must succeed" $ do
                            let
                                ctx' = ctx
                                -- NOTE: asi es como hay que hacer para probar las cosas si algo no funciona
                                -- !info = LedgerContextsV2.scriptContextTxInfo ctx
                                -- !outputs_txOuts = [ txOut | !txOut <- LedgerApiV2.txInfoOutputs info,
                                --             OnChainHelpers.isScriptAddress (LedgerApiV2.txOutAddress txOut)  ]
                                -- !investUnitID_AC = LedgerValue.AssetClass (tpFundPolicy_CS tp, T.investUnitID_TN)
                                -- !output_Own_TxOut_And_InvestUnitDatum =
                                --                 OnChainHelpers.getTxOut_And_DatumType_From_TxOut_And_AC_And_Address
                                --                     @InvestUnitT.ValidatorDatum @InvestUnitT.InvestUnitDatumType
                                --                     ctx
                                --                     (outputs_txOuts!!1)
                                --                     investUnitID_AC
                                --                     (Just (OffChainHelpers.addressValidator (tpInvestUnitValidator_Hash tp) ))
                                --                     InvestUnitT.getInvestUnit_DatumType
                                -- !inputs = LedgerApiV2.txInfoInputs info
                                -- !input_TxOut_BeingValidated = OnChainHelpers.getUnsafe_Own_Input_TxOut ctx
                                -- !investUnit_Validator_Address = LedgerApiV2.txOutAddress input_TxOut_BeingValidated
                                -- !scriptContextPurpose = LedgerContextsV2.scriptContextPurpose ctx

                                -- DebugTrace.trace ("outputs_txOuts: " ++ show outputs_txOuts) $
                                --     DebugTrace.trace ("output_Own_TxOut_And_InvestUnitDatum: " ++ show output_Own_TxOut_And_InvestUnitDatum) $
                                --     DebugTrace.trace ("input_TxOut_BeingValidated: " ++ show input_TxOut_BeingValidated) $
                                --     DebugTrace.trace ("investUnit_Validator_Address: " ++ show investUnit_Validator_Address) $
                                --     DebugTrace.trace ("inputs: " ++ show inputs) $
                                --     DebugTrace.trace ("scriptContextPurpose: " ++ show scriptContextPurpose) $
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                        , Tasty.testCase "Validation interval not having LowerBound must fail" $ do
                            let
                                ctx' = ctx
                                        |> setValidyRange
                                            ( LedgerApiV2.Interval
                                                (LedgerApiV2.LowerBound LedgerApiV2.NegInf False)
                                                (LedgerApiV2.strictUpperBound (tpDepositDate tp + 1))
                                            )
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                    `assertResultsContainAnyOf` ["Interval has no lower bound"]
                        , Tasty.testCase "Not including Protocol UTxO as InputRef must fail" $ do
                            let
                                ctx' = ctx
                                        |> setInputsRef [fund_UTxO_MockData tp]
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                    `assertResultsContainAnyOf` ["Expected exactly one Protocol input ref"]
                        , Tasty.testCase "InvestUnit tokens to add not having a price must fail" $ do
                            let
                                wrongAdaPrice = T.InvestUnit [("4E8D", "tokenB", 10)]
                                wrongOracleData = T.OracleReIdx_Data wrongAdaPrice (tpReIdxDate tp)
                                wrongPriceData = OnChainHelpers.oracleReIdxDataToBBS wrongOracleData
                                wrongRedeemer =
                                    InvestUnitT.mkReIndexingRedeemer
                                        investUnit_AfterReIdx
                                        investUnit_Initial
                                        wrongOracleData
                                        (Ledger.sign' wrongPriceData (tpOraclePrivateKey tp))
                                ctx' = ctx
                                        |> setInputsAndAddRedeemers [
                                            (fundHolding_UTxO_With_Deposits_MockData tp, FundHoldingT.mkReIndexingRedeemer investUnit_AfterReIdx investUnit_Initial),
                                            (investUnit_UTxO_MockData tp, wrongRedeemer)
                                            ]
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                    `assertResultsContainAnyOf` ["No price found for token"]
                        , Tasty.testCase "InvestUnit tokens to remove not having a price must fail" $ do
                            let
                                wrongAdaPrice = T.InvestUnit [("AF23", "tokenA", 20)]
                                wrongOracleData = T.OracleReIdx_Data wrongAdaPrice (tpReIdxDate tp)
                                wrongPriceData = OnChainHelpers.oracleReIdxDataToBBS wrongOracleData
                                wrongRedeemer =
                                    InvestUnitT.mkReIndexingRedeemer
                                        investUnit_AfterReIdx
                                        investUnit_Initial
                                        wrongOracleData
                                        (Ledger.sign' wrongPriceData (tpOraclePrivateKey tp))
                                ctx' = ctx
                                        |> setInputsAndAddRedeemers [
                                            (fundHolding_UTxO_With_Deposits_MockData tp, FundHoldingT.mkReIndexingRedeemer investUnit_AfterReIdx investUnit_Initial),
                                            (investUnit_UTxO_MockData tp, wrongRedeemer)
                                            ]
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                    `assertResultsContainAnyOf` ["No price found for token"]
                        , Tasty.testCase
                            "Add other prices in the list does not bother and must succeed" $ do
                            let
                                wrongAdaPrice =
                                    T.InvestUnit [("AF23", "tokenA", 20), ("4E8D", "tokenB", 10), ("4E8Dddde", "tokenC", 10)]
                                wrongOracleData = T.OracleReIdx_Data wrongAdaPrice (tpReIdxDate tp)
                                wrongPriceData = OnChainHelpers.oracleReIdxDataToBBS wrongOracleData
                                wrongRedeemer =
                                    InvestUnitT.mkReIndexingRedeemer
                                        investUnit_AfterReIdx
                                        investUnit_Initial
                                        wrongOracleData
                                        (Ledger.sign' wrongPriceData (tpOraclePrivateKey tp))
                                ctx' = ctx
                                        |> setInputsAndAddRedeemers [
                                            (fundHolding_UTxO_With_Deposits_MockData tp, FundHoldingT.mkReIndexingRedeemer investUnit_AfterReIdx investUnit_Initial),
                                            (investUnit_UTxO_MockData tp, wrongRedeemer)
                                            ]
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                        , Tasty.testCase
                            "Total price of tokens to add lower than tokens to remove must fail" $ do
                            let
                                wrongAdaPrice =
                                    T.InvestUnit [("AF23", "tokenA", 3), ("4E8D", "tokenB", 1)]
                                    -- tokenA voy a remover 500 y tokenB quiero agregar 1000
                                    -- total token precio a agregar : 1000 * 1 = 1000
                                    -- total token precio a remover : 500 * 3 = 1500
                                wrongOracleData = T.OracleReIdx_Data wrongAdaPrice (tpReIdxDate tp)
                                wrongPriceData = OnChainHelpers.oracleReIdxDataToBBS wrongOracleData
                                wrongRedeemer =
                                    InvestUnitT.mkReIndexingRedeemer
                                        investUnit_AfterReIdx
                                        investUnit_Initial
                                        wrongOracleData
                                        (Ledger.sign' wrongPriceData (tpOraclePrivateKey tp))
                                ctx' = ctx
                                        |> setInputsAndAddRedeemers [
                                            (fundHolding_UTxO_With_Deposits_MockData tp, FundHoldingT.mkReIndexingRedeemer investUnit_AfterReIdx investUnit_Initial),
                                            (investUnit_UTxO_MockData tp, wrongRedeemer)
                                            ]
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                    `assertResultsContainAnyOf` ["not isCorrect_Exchange_WithSamePriceADA"]
                        , Tasty.testCase
                            "Same Total price of tokens to add and tokens to remove must succeed" $ do
                            let
                                wrongAdaPrice =
                                    T.InvestUnit [("AF23", "tokenA", 2), ("4E8D", "tokenB", 1)]
                                    -- tokenA voy a remover 500 y tokenB quiero agregar 1000
                                    -- total token precio a agregar : 1000 * 1 = 1000
                                    -- total token precio a remover : 500 * 2 = 1000
                                oracleData = T.OracleReIdx_Data wrongAdaPrice (tpReIdxDate tp)
                                priceData = OnChainHelpers.oracleReIdxDataToBBS oracleData
                                redeemer =
                                    InvestUnitT.mkReIndexingRedeemer
                                        investUnit_AfterReIdx
                                        investUnit_Initial
                                        oracleData
                                        (Ledger.sign' priceData (tpOraclePrivateKey tp))
                                ctx' = ctx
                                        |> setInputsAndAddRedeemers [
                                            (fundHolding_UTxO_With_Deposits_MockData tp, FundHoldingT.mkReIndexingRedeemer investUnit_AfterReIdx investUnit_Initial),
                                            (investUnit_UTxO_MockData tp, redeemer)
                                            ]
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                        , Tasty.testCase
                            "Total price of tokens to remove lower than tokens to add must succeed" $ do
                            let
                                wrongAdaPrice =
                                    T.InvestUnit [("AF23", "tokenA", 1), ("4E8D", "tokenB", 1)]
                                    -- tokenA voy a remover 500 y tokenB quiero agregar 1000
                                    -- total tokenA precio a agregar : 1000 * 1 = 1000
                                    -- total tokenB precio a remover : 500 * 1 = 1500
                                oracleData = T.OracleReIdx_Data wrongAdaPrice (tpReIdxDate tp)
                                priceData = OnChainHelpers.oracleReIdxDataToBBS oracleData
                                redeemer =
                                    InvestUnitT.mkReIndexingRedeemer
                                        investUnit_AfterReIdx
                                        investUnit_Initial
                                        oracleData
                                        (Ledger.sign' priceData (tpOraclePrivateKey tp))
                                ctx' = ctx
                                        |> setInputsAndAddRedeemers [
                                            (fundHolding_UTxO_With_Deposits_MockData tp, FundHoldingT.mkReIndexingRedeemer investUnit_AfterReIdx investUnit_Initial),
                                            (investUnit_UTxO_MockData tp, redeemer)
                                            ]
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                        , Tasty.testCase "Including wrong InvestUnit in InvestUnit UTxO's Datum must fail" $ do
                            let
                                ctx' = ctx
                                        |> setOutputs [fundHolding_UTxO_After_Reidx_MockData tp investUnit_Initial investUnit_AfterReIdx, wrongInvestUnitUTxO]
                                wrongInvestUnitDatum =
                                    InvestUnitT.mkDatum $
                                        InvestUnitT.mkInvestUnit_DatumType
                                            (tpFundPolicy_CS tp)
                                            investUnit_Initial
                                            minAdaIUDatum
                                wrongInvestUnitUTxO =
                                    LedgerApiV2.TxOut
                                        (OffChainHelpers.addressValidator (tpInvestUnitValidator_Hash tp))
                                        ( LedgerAda.lovelaceValueOf minAdaIUDatum
                                            <> LedgerApiV2.singleton (tpFundPolicy_CS tp) T.investUnitID_TN 1
                                        )
                                        (LedgerApiV2.OutputDatum wrongInvestUnitDatum)
                                        Nothing
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                    `assertResultsContainAnyOf` ["not isCorrect_Output_InvestUnit_Datum_WithTokensExchanged"]
                        , Tasty.testCase "Changing the value of the output InvestUnit UTxO must fail" $ do
                            let
                                wrongUTxO =
                                    (investUnit_UTxO_After_ReIdx_MockData tp)
                                        { LedgerApiV2.txOutValue =
                                            toAlter_Value_Adding_SomeADA
                                                <> LedgerApiV2.txOutValue (investUnit_UTxO_After_ReIdx_MockData tp)
                                        }
                                ctx' = ctx
                                        |> setOutputs [fundHolding_UTxO_After_Reidx_MockData tp investUnit_Initial investUnit_AfterReIdx, wrongUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                    `assertResultsContainAnyOf` ["not isCorrect_Output_InvestDatum_Value_NotChanged"]
                        , Tasty.testCase "Using an incorrect Oracle signature must fail" $ do
                            let
                                wrongOraclePrivateKey =
                                    Ledger.generateFromSeed' $
                                        OffChainHelpers.stringToStrictByteString
                                            "eternity xenon pyramid octopus vivid binary rhyme mural \
                                            \artisan voyage harmony nomad novel delta abstract rally \
                                            \spectrum tundra visionary voyage frequency terminal \
                                            \serendipity meadow"
                                wrongRedeemer =
                                    -- InvestUnitT.mkReIndexingRedeemer investUnit_AfterReIdx investUnit_Initial (oracleReIdxData tp) (oracleReIdxSignature tp)
                                    InvestUnitT.mkReIndexingRedeemer
                                        investUnit_AfterReIdx
                                        investUnit_Initial
                                        (oracleReIdxData tp) --LedgerCrypto.sign' (OnChainHelpers.oracleReIdxDataToBBS (oracleReIdxData tp)) (tpOraclePrivateKey tp)
                                        (LedgerCrypto.sign' (OnChainHelpers.oracleReIdxDataToBBS (oracleReIdxData tp)) wrongOraclePrivateKey)
                                ctx' = ctx
                                        |> setInputsAndAddRedeemers [
                                            (fundHolding_UTxO_With_Deposits_MockData tp, FundHoldingT.mkReIndexingRedeemer investUnit_AfterReIdx investUnit_Initial),
                                            (investUnit_UTxO_MockData tp, wrongRedeemer)
                                            ]
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                    `assertResultsContainAnyOf` ["not isCorrect_Oracle_Signature"]
                        , Tasty.testCase
                            "Using an incorrect Oracle time range must fail" $ do
                            let
                                -- the ranne of the tx is check, and is between the validTimeRange for tx
                                -- validTimeRange = 900_000 -- 15 * 60 * 1000 = 15 minutos
                                -- then from the lower bound of the tx range, the oracleData_Valid_Time is subtracted
                                -- oracleData_Valid_Time = 300_000 -- 5 * 60 * 1000 = 5 minutos
                                -- and the valid range goes from that value to the upper bound of the tx range
                                -- in this tx, the tx range is created with
                                -- LedgerInterval.interval
                                -- from: (date'  - LedgerApiV2.POSIXTime (LedgerApiV2.getPOSIXTime T.validTimeRange `divide` 2) + 1)
                                -- to: (date' + LedgerApiV2.POSIXTime (LedgerApiV2.getPOSIXTime T.validTimeRange `divide` 2) -1)
                                -- so for creating a invalid range we use the lower bound, we subtract the oracleData_Valid_Time and we substract 1 more
                                validLowerBound =  (tpReIdxDate tp  - LedgerApiV2.POSIXTime (LedgerApiV2.getPOSIXTime T.validTimeRange `divide` 2) + 1) - T.oracleData_Valid_Time
                                invalidLowerBound = validLowerBound - 1
                                wrongOracleData = T.OracleReIdx_Data tokensReIdxPrice invalidLowerBound
                                wrongPriceData = OnChainHelpers.oracleReIdxDataToBBS wrongOracleData
                                wrongRedeemer =
                                    InvestUnitT.mkReIndexingRedeemer
                                        investUnit_AfterReIdx
                                        investUnit_Initial
                                        wrongOracleData
                                        (Ledger.sign' wrongPriceData (tpOraclePrivateKey tp))
                                ctx' = ctx
                                        |> setInputsAndAddRedeemers [
                                            (fundHolding_UTxO_With_Deposits_MockData tp, FundHoldingT.mkReIndexingRedeemer investUnit_AfterReIdx investUnit_Initial),
                                            (investUnit_UTxO_MockData tp, wrongRedeemer)
                                            ]
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                    `assertResultsContainAnyOf` ["not isCorrect_Oracle_InRangeTime"]
                        , Tasty.testCase "Not including FundHolding ReIndexing Redeemer must fail" $ do
                            let
                                wrongRedeemer =
                                    FundHoldingT.mkUpdateMinADARedeemer
                                ctx' = ctx
                                        |> setInputsAndAddRedeemers [
                                                (fundHolding_UTxO_With_Deposits_MockData tp, wrongRedeemer),
                                                (investUnit_UTxO_MockData tp, InvestUnitT.mkReIndexingRedeemer investUnit_AfterReIdx investUnit_Initial (oracleReIdxData tp)  (oracleReIdxSignature tp))
                                            ]
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                    `assertResultsContainAnyOf` ["not isCorrect_Redeemer_FundHolding"]
                        , Tasty.testCase "Not including Fund UTxO as InputRef must fail" $ do
                            let
                                ctx' = ctx
                                        |> setInputsRef [protocol_UTxO_MockData tp]
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                    `assertResultsContainAnyOf` ["Expected exactly one Fund input ref"]
                        , Tasty.testCase "Not including FundHolding UTxO as Input must fail" $ do
                            let
                                ctx' = ctx
                                        |> setInputsAndAddRedeemers [
                                            (investUnit_UTxO_MockData tp, InvestUnitT.mkReIndexingRedeemer investUnit_AfterReIdx investUnit_Initial (oracleReIdxData tp)  (oracleReIdxSignature tp))
                                            ]
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                    `assertResultsContainAnyOf` ["Expected exactly one FundHolding input"]
                        ]

--------------------------------------------------------------------------------

investUnit_Validator_Redeemer_UpdateMinADA_Tests :: TestParams -> Tasty.TestTree
investUnit_Validator_Redeemer_UpdateMinADA_Tests tp =
    let ------------------------
        txName = show InvestUnit_UpdateMinADA_Tx
        selectedRedeemer = RedeemerLogValidator (Just InvestUnit_UpdateMinADA_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = investUnit_UpdateMinADA_TxContext tp toAlter_minAda
                in
                    [
                        Tasty.testCase "Changing min ADA correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                    ]

--------------------------------------------------------------------------------
