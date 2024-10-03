--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4
{- |
Module      : Tests.PropertyBasedTests.InvestUnit.Validator
Description : Validation logic and property-based tests related to the
              InvestUnit validator.

This module defines the validation logic for the Invest Unit contract.

It includes multiple property-based test cases to ensure the integrity and
correctness of the validator script.
-}
module Contracts.InvestUnit.Validator where
--------------------------------------------------------------------------------
-- Non-IOG imports
import qualified Control.Monad.Reader                      as MReader
import           Prelude
import qualified Test.Tasty                                as Tasty
import qualified Test.Tasty.QuickCheck                     as TastyQC

-- IOG imports
import qualified Ledger
import qualified Plutus.V2.Ledger.Api                      as LedgerApiV2
import           PlutusTx.Prelude                          ()

-- Project imports
import qualified Generic.OnChainHelpers                    as OnChainHelpers
import qualified Protocol.Fund.Holding.Types               as FundHoldingT
import qualified Protocol.InvestUnit.Types                 as InvestUnitT
import qualified Protocol.OnChainHelpers                   as OnChainHelpers (oracleReIdxDataToBBS)
import qualified Protocol.Types                            as T
import qualified Test.QuickCheck                           as QC
import           TestUtils.Contracts.InitialData
import           TestUtils.Contracts.TxContext.InvestUnit
import           TestUtils.HelpersMAYZ
import           TestUtils.QuickCheckGen.QuickCheckGen
import           TestUtils.QuickCheckGen.QuickCheckGenMAYZ
import           TestUtils.TestContext.Asserts
import           TestUtils.TestContext.Helpers
import           TestUtils.Types
import           TestUtils.TypesMAYZ

-------------------------------------------------------------------------------

investUnit_Validator_Tests :: AppM Tasty.TestTree
investUnit_Validator_Tests = do
    tp <- MReader.asks appTestParams
    return $
        Tasty.testGroup
            "InvestUnit Validator Tests"
        [
            investUnit_Validator_Redeemer_ReIndexing_Tests tp
            , investUnit_Validator_Redeemer_UpdateMinADA_Tests tp
            , investUnit_Validator_Redeemer_Delete_Tests tp
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
                        TastyQC.testProperty
                            "ReIndexing varying initial invest unit, tokens to add, tokens to remove and prices should succeed"
                            (prop_reIndex_OK tp selectedRedeemer ctx)
                        , TastyQC.testProperty
                            "ReIndexing changing the invest unit value must fail"
                            (prop_reIndex_changeValueFail tp selectedRedeemer ctx)
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
                    --TODO:
                    ctx = investUnit_UpdateMinADA_TxContext tp toAlter_minAda
                in
                    []

--------------------------------------------------------------------------------

investUnit_Validator_Redeemer_Delete_Tests :: TestParams -> Tasty.TestTree
investUnit_Validator_Redeemer_Delete_Tests tp =
    let ------------------------
        txName = show Fund_Delete_Tx
        selectedRedeemer = RedeemerLogValidator (Just InvestUnit_Delete_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    --TODO:
                    ctx = investUnit_Delete_TxContext tp
                in
                    []


--------------------------------------------------------------------------------

prop_reIndex_OK :: TestParams -> RedeemerLog -> LedgerApiV2.ScriptContext -> T.InvestUnit -> T.InvestUnit -> QC.Property
prop_reIndex_OK tp selectedRedeemer ctx inputIU addIU =
    QC.forAll randomPriceAndIU $ \(tokensPrice', removeIU) -> do
        let
            oracleData' = T.OracleReIdx_Data tokensPrice' (tpReIdxDate tp)
            priceData' = OnChainHelpers.oracleReIdxDataToBBS oracleData'
            oracleSignature' = Ledger.sign' priceData' (tpOraclePrivateKey tp)

            input_InvestUnit_Datum =
                InvestUnitT.mkDatum $
                    InvestUnitT.mkInvestUnit_DatumType
                        (tpFundPolicy_CS tp)
                        inputIU
                        minAdaIUDatum
            input_InvestUnit_UTxO = (investUnit_UTxO_MockData tp) {LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum input_InvestUnit_Datum}

            output_InvestUnit' = OnChainHelpers.flattenValueAdd (T.iuValues inputIU) (T.iuValues addIU)
            output_InvestUnit = OnChainHelpers.flattenValueSub output_InvestUnit' (T.iuValues removeIU)

            output_InvestUnit_Datum =
                InvestUnitT.mkDatum $
                    InvestUnitT.mkInvestUnit_DatumType
                        (tpFundPolicy_CS tp)
                        (T.InvestUnit output_InvestUnit)
                        minAdaIUDatum
            output_InvestUnit_UTxO = (investUnit_UTxO_MockData tp) {LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum output_InvestUnit_Datum}

            ctx'= ctx
                    |> setInputsRef [protocol_UTxO_MockData tp, fund_UTxO_With_Added_FundHolding_MockData tp]
                    |> setInputsAndAddRedeemers
                        [ ( input_InvestUnit_UTxO
                          , InvestUnitT.mkReIndexingRedeemer addIU removeIU oracleData' oracleSignature'
                          ),
                          ( fundHolding_UTxO_With_Deposits_MockData tp
                          , FundHoldingT.mkReIndexingRedeemer addIU removeIU
                          )
                        ]
                    |> setOutputs [fundHolding_UTxO_After_Reidx_MockData tp addIU removeIU, output_InvestUnit_UTxO]
                    |> setSignatories (tpFundAdmins tp)
                    |> setValidyRange (createValidRange (tpReIdxDate tp))
        results <- testContextWrapper tp ctx'
        (Just selectedRedeemer, results)
            `assertResultsContainAnyOf` []
    where
        randomPriceAndIU :: QC.Gen (T.InvestUnit, T.InvestUnit)
        randomPriceAndIU = do
            removeIU' <-
                QC.arbitrary
                    `QC.suchThat` ( \iu2 -> head (getAssets $ T.iuValues iu2) `notElem` getAssets (T.iuValues addIU)
                                  )
            -- We force the first amount to be 1, so it can always balance the total price.
            let
                removeIU =
                    let
                        (cs, tn, _) = head (T.iuValues removeIU')
                    in
                        T.InvestUnit $ (cs, tn, 1) : tail (T.iuValues removeIU')
            randomPrice <- getRandomPrice addIU removeIU
            return (randomPrice, removeIU)
        getAssets = map (\(cs, tn, _) -> (cs, tn))

prop_reIndex_changeValueFail :: TestParams -> RedeemerLog -> LedgerApiV2.ScriptContext -> T.InvestUnit -> T.InvestUnit -> QC.Property
prop_reIndex_changeValueFail tp selectedRedeemer ctx inputIU addIU =
    QC.forAll randomPriceIUAndValue $ \(tokensPrice', removeIU, value) -> do
        let
            oracleData' = T.OracleReIdx_Data tokensPrice' (tpReIdxDate tp)
            priceData' = OnChainHelpers.oracleReIdxDataToBBS oracleData'
            oracleSignature' = Ledger.sign' priceData' (tpOraclePrivateKey tp)

            input_InvestUnit_Datum =
                InvestUnitT.mkDatum $
                    InvestUnitT.mkInvestUnit_DatumType
                        (tpFundPolicy_CS tp)
                        inputIU
                        2_000_000
            input_InvestUnit_UTxO = (investUnit_UTxO_MockData tp) {LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum input_InvestUnit_Datum}

            output_InvestUnit' = OnChainHelpers.flattenValueAdd (T.iuValues inputIU) (T.iuValues addIU)
            output_InvestUnit = OnChainHelpers.flattenValueSub output_InvestUnit' (T.iuValues removeIU)

            output_InvestUnit_Datum =
                InvestUnitT.mkDatum $
                    InvestUnitT.mkInvestUnit_DatumType
                        (tpFundPolicy_CS tp)
                        (T.InvestUnit output_InvestUnit)
                        2_000_000
            output_InvestUnit_UTxO =
                (investUnit_UTxO_MockData tp)
                    { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum output_InvestUnit_Datum
                    , LedgerApiV2.txOutValue = LedgerApiV2.txOutValue (investUnit_UTxO_MockData tp) <> value
                    }
            ctx'= ctx
                    |> setInputsRef [protocol_UTxO_MockData tp, fund_UTxO_MockData tp]
                    |> setInputsAndAddRedeemers
                        [ ( input_InvestUnit_UTxO
                          , InvestUnitT.mkReIndexingRedeemer addIU removeIU oracleData' oracleSignature'
                          ),
                          ( fundHolding_UTxO_With_Deposits_MockData tp
                          , FundHoldingT.mkReIndexingRedeemer addIU removeIU
                          )
                        ]
                    |> setOutputs [fundHolding_UTxO_After_Reidx_MockData tp addIU removeIU , output_InvestUnit_UTxO]
                    |> setSignatories (tpFundAdmins tp)
                    |> setValidyRange (createValidRange (tpReIdxDate tp))
        results <- testContextWrapper tp ctx'
        (Just selectedRedeemer, results)
            `assertResultsContainAnyOf` ["not isCorrect_Output_InvestUnit_Value_NotChanged"]
    where
        randomPriceIUAndValue :: QC.Gen (T.InvestUnit, T.InvestUnit, LedgerApiV2.Value)
        randomPriceIUAndValue = do
            removeIU' <-
                QC.arbitrary
                    `QC.suchThat` ( \iu2 ->  head (getAssets $ T.iuValues iu2) `notElem` getAssets (T.iuValues addIU)
                                  )
            -- We force the first amount to be 1, so it can always balance the total price.
            let
                removeIU =
                    let
                        (cs, tn, _) = head (T.iuValues removeIU')
                    in
                        T.InvestUnit $ (cs, tn, 1) : tail (T.iuValues removeIU')
            randomPrice <- getRandomPrice addIU removeIU
            randomValue <- varyValue $ LedgerApiV2.txOutValue (investUnit_UTxO_MockData tp)
            return (randomPrice, removeIU, randomValue)
        getAssets = map (\(cs, tn, _) -> (cs, tn))
