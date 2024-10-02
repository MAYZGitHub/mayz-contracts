--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4

module Contracts.Protocol.MintingPolicy where
--------------------------------------------------------------------------------
-- Non-IOG imports
import           Prelude                                (show)
import qualified Test.Tasty                             as Tasty
import qualified Test.Tasty.HUnit                       as Tasty

-- IOG imports
import qualified Plutus.V2.Ledger.Api                   as LedgerApiV2
import           PlutusTx.Prelude

-- Project imports
import qualified Protocol.Protocol.Types                as ProtocolT
import           TestUtils.Contracts.InitialData
import           TestUtils.Contracts.TxContext.Protocol
import           TestUtils.HelpersMAYZ
import           TestUtils.TestContext.Asserts
import           TestUtils.TestContext.Helpers
import           TestUtils.Types
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------

protocol_Policy_Tests :: TestParams -> Tasty.TestTree
protocol_Policy_Tests tp =
    Tasty.testGroup
        "Protocol Policy Tests"
        [
            protocol_Policy_Redeemer_MintID_Tests tp
        ]

--------------------------------------------------------------------------------

protocol_Policy_Redeemer_MintID_Tests :: TestParams -> Tasty.TestTree
protocol_Policy_Redeemer_MintID_Tests tp =
    let
        ------------------------
        txName = show Protocol_Create_Tx
        selectedRedeemer = RedeemerLogPolicy (Just Protocol_MintID_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = protocol_Create_TxContext tp
                in
                    [
                        Tasty.testCase "Minting ID correctly must succeed" $ do
                            let
                                ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                    `assertResultsContainAnyOf` [],
                        -- Test case for "not length fundCategories > 0"
                        Tasty.testCase "Invalid fund categories length must fail" $ do
                            let
                                outputDatum = (protocol_DatumType_MockData tp) {ProtocolT.pdFundCategories = []}
                                outputProtocolUTxO = (protocol_UTxO_MockData tp) {LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ ProtocolT.mkDatum outputDatum}
                                ctx' = ctx |> setOutputs [outputProtocolUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogPolicy (Just Protocol_MintID_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not length fundCategories > 0"],

                        -- Test case for "not fundCategories fcRequiredMAYZ >= 0"
                        Tasty.testCase "Invalid fund category MAYZ requirement must fail" $ do
                            let
                                outputDatum = (protocol_DatumType_MockData tp)
                                                { ProtocolT.pdFundCategories = [ProtocolT.FundCategory {ProtocolT.fcRequiredMAYZ = -1, ProtocolT.fcCategoryNumber = 0, ProtocolT.fcMaxUI = 1}] }
                                outputProtocolUTxO = (protocol_UTxO_MockData tp) {LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ ProtocolT.mkDatum outputDatum}
                                ctx' = ctx |> setOutputs [outputProtocolUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogPolicy (Just Protocol_MintID_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not fundCategories fcRequiredMAYZ >= 0"],

                        -- Test case for "not fundCategories fcMaxUI >= 0"
                        Tasty.testCase "Invalid fund category MaxUI requirement must fail" $ do
                            let
                                outputDatum = (protocol_DatumType_MockData tp)
                                                { ProtocolT.pdFundCategories = [ProtocolT.FundCategory {ProtocolT.fcRequiredMAYZ = 0, ProtocolT.fcCategoryNumber = 0, ProtocolT.fcMaxUI = -1}] }
                                outputProtocolUTxO = (protocol_UTxO_MockData tp) {LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ ProtocolT.mkDatum outputDatum}
                                ctx' = ctx |> setOutputs [outputProtocolUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogPolicy (Just Protocol_MintID_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not fundCategories fcMaxUI >= 0"],

                        -- Test case for "not fundCategories have unique and sequential category numbers"
                        Tasty.testCase "Non-sequential or non-unique category numbers must fail" $ do
                            let
                                outputDatum = (protocol_DatumType_MockData tp)
                                                { ProtocolT.pdFundCategories = [ProtocolT.FundCategory {ProtocolT.fcCategoryNumber = 0, ProtocolT.fcRequiredMAYZ = 0, ProtocolT.fcMaxUI = 1}
                                                                            , ProtocolT.FundCategory {ProtocolT.fcCategoryNumber = 0, ProtocolT.fcRequiredMAYZ = 0, ProtocolT.fcMaxUI = 1}] }
                                outputProtocolUTxO = (protocol_UTxO_MockData tp) {LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ ProtocolT.mkDatum outputDatum}
                                ctx' = ctx |> setOutputs [outputProtocolUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogPolicy (Just Protocol_MintID_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not fundCategories have unique and sequential category numbers"],

                        -- Test case for "not requiredMAYZForSellOffer >= 0"
                        Tasty.testCase "Invalid required MAYZ for sell offer must fail" $ do
                            let
                                outputDatum = (protocol_DatumType_MockData tp) {ProtocolT.pdRequiredMAYZForSellOffer = -1}
                                outputProtocolUTxO = (protocol_UTxO_MockData tp) {LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ ProtocolT.mkDatum outputDatum}
                                ctx' = ctx |> setOutputs [outputProtocolUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogPolicy (Just Protocol_MintID_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not requiredMAYZForSellOffer >= 0"],

                        -- Test case for "not requiredMAYZForBuyOrder >= 0"
                        Tasty.testCase "Invalid required MAYZ for buy order must fail" $ do
                            let
                                outputDatum = (protocol_DatumType_MockData tp) {ProtocolT.pdRequiredMAYZForBuyOrder = -1}
                                outputProtocolUTxO = (protocol_UTxO_MockData tp) {LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ ProtocolT.mkDatum outputDatum}
                                ctx' = ctx |> setOutputs [outputProtocolUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogPolicy (Just Protocol_MintID_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not requiredMAYZForBuyOrder >= 0"],

                        -- Test case for "not isValidMinMaxDef fundLifeTime"
                        Tasty.testCase "Invalid fund lifetime must fail" $ do
                            let
                                outputDatum = (protocol_DatumType_MockData tp) {ProtocolT.pdFundLifeTime = ProtocolT.MinMaxDef {ProtocolT.mmdMin = 0, ProtocolT.mmdMax = 1000, ProtocolT.mmdDef = -1}}
                                outputProtocolUTxO = (protocol_UTxO_MockData tp) {LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ ProtocolT.mkDatum outputDatum}
                                ctx' = ctx |> setOutputs [outputProtocolUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogPolicy (Just Protocol_MintID_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not isValidMinMaxDef fundLifeTime"],

                        -- Test case for "not Min fundLifeTime >= 0"
                        Tasty.testCase "Invalid fund lifetime minimum must fail" $ do
                            let
                                outputDatum = (protocol_DatumType_MockData tp) {ProtocolT.pdFundLifeTime = ProtocolT.MinMaxDef {ProtocolT.mmdMin = -1, ProtocolT.mmdMax = 1000, ProtocolT.mmdDef = 0}}
                                outputProtocolUTxO = (protocol_UTxO_MockData tp) {LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ ProtocolT.mkDatum outputDatum}
                                ctx' = ctx |> setOutputs [outputProtocolUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogPolicy (Just Protocol_MintID_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not Min fundLifeTime >= 0"],

                        -- Test case for "not Min commissionFund_PerYear_InBPx1e3 >= 0"
                        Tasty.testCase "Invalid minimum commission fund per year must fail" $ do
                            let
                                outputDatum = (protocol_DatumType_MockData tp) {ProtocolT.pdCommissionFund_PerYear_InBPx1e3 = ProtocolT.MinMaxDef {ProtocolT.mmdMin = -1, ProtocolT.mmdMax = 1000, ProtocolT.mmdDef = 0}}
                                outputProtocolUTxO = (protocol_UTxO_MockData tp) {LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ ProtocolT.mkDatum outputDatum}
                                ctx' = ctx |> setOutputs [outputProtocolUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogPolicy (Just Protocol_MintID_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not Min commissionFund_PerYear_InBPx1e3 >= 0"],

                        -- Test case for "not Max commissionFund_PerYear_InBPx1e3 <= 100%"
                        Tasty.testCase "Invalid maximum commission fund per year must fail" $ do
                            let
                                outputDatum = (protocol_DatumType_MockData tp) {ProtocolT.pdCommissionFund_PerYear_InBPx1e3 = ProtocolT.MinMaxDef {ProtocolT.mmdMin = 0, ProtocolT.mmdMax = 10_000_001, ProtocolT.mmdDef = 0}}
                                outputProtocolUTxO = (protocol_UTxO_MockData tp) {LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ ProtocolT.mkDatum outputDatum}
                                ctx' = ctx |> setOutputs [outputProtocolUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogPolicy (Just Protocol_MintID_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not Max commissionFund_PerYear_InBPx1e3 <= 100%"],

                        -- Test case for "not Min commissionSellOffer_InBPx1e3 >= 0"
                        Tasty.testCase "Invalid minimum commission for sell offer must fail" $ do
                            let
                                outputDatum = (protocol_DatumType_MockData tp) {ProtocolT.pdCommissionSellOffer_InBPx1e3 = ProtocolT.MinMaxDef {ProtocolT.mmdMin = -1, ProtocolT.mmdMax = 1000, ProtocolT.mmdDef = 0}}
                                outputProtocolUTxO = (protocol_UTxO_MockData tp) {LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ ProtocolT.mkDatum outputDatum}
                                ctx' = ctx |> setOutputs [outputProtocolUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogPolicy (Just Protocol_MintID_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not Min commissionSellOffer_InBPx1e3 >= 0"],

                        -- Test case for "not Max commissionSellOffer_InBPx1e3 <= 100%"
                        Tasty.testCase "Invalid maximum commission for sell offer must fail" $ do
                            let
                                outputDatum = (protocol_DatumType_MockData tp) {ProtocolT.pdCommissionSellOffer_InBPx1e3 = ProtocolT.MinMaxDef {ProtocolT.mmdMin = 0, ProtocolT.mmdMax = 10_000_001, ProtocolT.mmdDef = 0}}
                                outputProtocolUTxO = (protocol_UTxO_MockData tp) {LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ ProtocolT.mkDatum outputDatum}
                                ctx' = ctx |> setOutputs [outputProtocolUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogPolicy (Just Protocol_MintID_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not Max commissionSellOffer_InBPx1e3 <= 100%"],

                        -- Test case for "not Min commissionBuyOrder_InBPx1e3 >= 0"
                        Tasty.testCase "Invalid minimum commission for buy order must fail" $ do
                            let
                                outputDatum = (protocol_DatumType_MockData tp) {ProtocolT.pdCommissionBuyOrder_InBPx1e3 = ProtocolT.MinMaxDef {ProtocolT.mmdMin = -1, ProtocolT.mmdMax = 1000, ProtocolT.mmdDef = 0}}
                                outputProtocolUTxO = (protocol_UTxO_MockData tp) {LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ ProtocolT.mkDatum outputDatum}
                                ctx' = ctx |> setOutputs [outputProtocolUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogPolicy (Just Protocol_MintID_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not Min commissionBuyOrder_InBPx1e3 >= 0"],

                        -- Test case for "not Max commissionBuyOrder_InBPx1e3 <= 100%"
                        Tasty.testCase "Invalid maximum commission for buy order must fail" $ do
                            let
                                outputDatum = (protocol_DatumType_MockData tp) {ProtocolT.pdCommissionBuyOrder_InBPx1e3 = ProtocolT.MinMaxDef {ProtocolT.mmdMin = 0, ProtocolT.mmdMax = 10_000_001, ProtocolT.mmdDef = 0}}
                                outputProtocolUTxO = (protocol_UTxO_MockData tp) {LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ ProtocolT.mkDatum outputDatum}
                                ctx' = ctx |> setOutputs [outputProtocolUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogPolicy (Just Protocol_MintID_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not Max commissionBuyOrder_InBPx1e3 <= 100%"],

                        -- Test case for "not share_InBPx1e2_Protocol + share_InBPx1e2_Delegators + share_InBPx1e2_Managers = 1_000_000 BPx1e2 = 100%"
                        Tasty.testCase "Invalid share distribution must fail" $ do
                            let
                                outputDatum = (protocol_DatumType_MockData tp)
                                                { ProtocolT.pdShare_InBPx1e2_Protocol = 300000
                                                , ProtocolT.pdShare_InBPx1e2_Delegators = 300000
                                                , ProtocolT.pdShare_InBPx1e2_Managers = 300000
                                                }
                                outputProtocolUTxO = (protocol_UTxO_MockData tp) {LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ ProtocolT.mkDatum outputDatum}
                                ctx' = ctx |> setOutputs [outputProtocolUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogPolicy (Just Protocol_MintID_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not share_InBPx1e2_Protocol + share_InBPx1e2_Delegators + share_InBPx1e2_Managers = 1_000_000 BPx1e2 = 100%"]
                    ]


--------------------------------------------------------------------------------
