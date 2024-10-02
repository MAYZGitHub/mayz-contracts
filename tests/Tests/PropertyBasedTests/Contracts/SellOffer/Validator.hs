--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4

{- |
Module      : SellOffer.Validator
Description :
-}
module Contracts.SellOffer.Validator where
--------------------------------------------------------------------------------
-- Non-IOG imports
import qualified Control.Monad.Reader                    as MReader
import           Prelude                                 (show)
import qualified Test.Tasty                              as Tasty
-- IOG imports
import           PlutusTx.Prelude

-- Project imports
import           TestUtils.Contracts.InitialData
import           TestUtils.Contracts.TxContext.SellOffer
import           TestUtils.TypesMAYZ
import qualified Protocol.Constants as T
import qualified Protocol.Protocol.Types as ProtocolT

--------------------------------------------------------------------------------

sellOffer_Validator_Tests :: AppM Tasty.TestTree
sellOffer_Validator_Tests = do
    tp <- MReader.asks appTestParams
    return $
        Tasty.testGroup
            "SellOffer Validator Tests"
            [ sellOffer_Validator_Redeemer_UpdateStatus_Tests tp
            , sellOffer_Validator_Redeemer_UpdateAskedCommissionRate_Tests tp
            , sellOffer_Validator_Redeemer_UpdateSellRestrictions_Tests tp
            , sellOffer_Validator_Redeemer_UpdateMinADA_Tests tp
            , sellOffer_Validator_Redeemer_Deposit_Tests tp
            , sellOffer_Validator_Redeemer_Withdraw_Tests tp
            , sellOffer_Validator_Redeemer_SwapFTxADA_Tests tp
            , sellOffer_Validator_Redeemer_SwapADAxFT_Tests tp
            , sellOffer_Validator_Redeemer_Delete_Tests tp
            ]

--------------------------------------------------------------------------------

sellOffer_Validator_Redeemer_UpdateStatus_Tests :: TestParams -> Tasty.TestTree
sellOffer_Validator_Redeemer_UpdateStatus_Tests tp =
    let
        ------------------------
        txName = show SellOffer_UpdateStatus_Tx
        selectedRedeemer = RedeemerLogValidator (Just SellOffer_UpdateStatus_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = sellOffer_UpdateStatus_TxContext tp T.sellOffer_Status_Open
                in
                    [
                    ]

--------------------------------------------------------------------------------

sellOffer_Validator_Redeemer_UpdateAskedCommissionRate_Tests :: TestParams -> Tasty.TestTree
sellOffer_Validator_Redeemer_UpdateAskedCommissionRate_Tests tp =
    let
        ------------------------
        txName = show SellOffer_UpdateAskedCommissionRate_Tx
        selectedRedeemer = RedeemerLogValidator (Just SellOffer_UpdateAskedCommissionRate_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = sellOffer_UpdateAskedCommissionRate_TxContext tp (ProtocolT.mmdMin $ ProtocolT.pdCommissionSellOffer_InBPx1e3 $ protocol_DatumType_MockData tp) 
                in
                    [
                    ]

--------------------------------------------------------------------------------

sellOffer_Validator_Redeemer_UpdateSellRestrictions_Tests :: TestParams -> Tasty.TestTree
sellOffer_Validator_Redeemer_UpdateSellRestrictions_Tests tp =
    let
        ------------------------
        txName = show SellOffer_UpdateSellRestrictions_Tx
        selectedRedeemer = RedeemerLogValidator (Just SellOffer_UpdateSellRestrictions_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = sellOffer_UpdateSellRestrictions_TxContext tp T.sellOffer_NotAllowSell T.sellOffer_NotAllowSell
                in
                    [
                    ]

--------------------------------------------------------------------------------

sellOffer_Validator_Redeemer_UpdateMinADA_Tests :: TestParams -> Tasty.TestTree
sellOffer_Validator_Redeemer_UpdateMinADA_Tests tp =
    let
        ------------------------
        txName = show SellOffer_UpdateMinADA_Tx
        selectedRedeemer = RedeemerLogValidator (Just SellOffer_UpdateMinADA_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = sellOffer_UpdateMinADA_TxContext tp toAlter_minAda
                in
                    [
                    ]

--------------------------------------------------------------------------------

sellOffer_Validator_Redeemer_Deposit_Tests :: TestParams -> Tasty.TestTree
sellOffer_Validator_Redeemer_Deposit_Tests tp =
    let
        ------------------------
        txName = show SellOffer_Deposit_Tx
        selectedRedeemer = RedeemerLogValidator (Just SellOffer_Deposit_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = sellOffer_Deposit_TxContext tp
                in
                    [
                    ]

--------------------------------------------------------------------------------

sellOffer_Validator_Redeemer_Withdraw_Tests :: TestParams -> Tasty.TestTree
sellOffer_Validator_Redeemer_Withdraw_Tests tp =
    let
        ------------------------
        txName = show SellOffer_Withdraw_Tx
        selectedRedeemer = RedeemerLogValidator (Just SellOffer_Withdraw_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = sellOffer_Withdraw_TxContext tp
                in
                    [
                    ]

--------------------------------------------------------------------------------

sellOffer_Validator_Redeemer_SwapFTxADA_Tests :: TestParams -> Tasty.TestTree
sellOffer_Validator_Redeemer_SwapFTxADA_Tests tp =
    let
        ------------------------
        txName = show SellOffer_SwapFTxADA_Tx
        selectedRedeemer = RedeemerLogValidator (Just SellOffer_SwapFTxADA_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = sellOffer_SwapFTxADA_TxContext tp
                in
                    [
                    ]

--------------------------------------------------------------------------------

sellOffer_Validator_Redeemer_SwapADAxFT_Tests :: TestParams -> Tasty.TestTree
sellOffer_Validator_Redeemer_SwapADAxFT_Tests tp =
    let
        ------------------------
        txName = show SellOffer_SwapADAxFT_Tx
        selectedRedeemer = RedeemerLogValidator (Just SellOffer_SwapADAxFT_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = sellOffer_SwapADAxFT_TxContext tp
                in
                    [
                    ]

--------------------------------------------------------------------------------

sellOffer_Validator_Redeemer_Delete_Tests :: TestParams -> Tasty.TestTree
sellOffer_Validator_Redeemer_Delete_Tests tp =
    let
        ------------------------
        txName = show SellOffer_Delete_Tx
        selectedRedeemer = RedeemerLogValidator (Just SellOffer_Delete_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = sellOffer_Delete_TxContext tp
                in
                    [
                    ]

--------------------------------------------------------------------------------

