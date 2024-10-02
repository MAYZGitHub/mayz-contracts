--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4

{- |
Module      : SwapOffer.Validator
Description :
-}
module Contracts.SwapOffer.Validator where
--------------------------------------------------------------------------------
-- Non-IOG imports
import qualified Control.Monad.Reader                    as MReader
import           Prelude                                 (show)
import qualified Test.Tasty                              as Tasty
-- IOG imports
import           PlutusTx.Prelude

-- Project imports
import qualified Protocol.Constants                      as T
import qualified Protocol.Protocol.Types                 as ProtocolT
import           TestUtils.Contracts.InitialData
import           TestUtils.Contracts.TxContext.SwapOffer
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------

swapOffer_Validator_Tests :: AppM Tasty.TestTree
swapOffer_Validator_Tests = do
    tp <- MReader.asks appTestParams
    return $
        Tasty.testGroup
            "SwapOffer Validator Tests"
            [ swapOffer_Validator_Redeemer_UpdateStatus_Tests tp
            , swapOffer_Validator_Redeemer_UpdateAskedCommissionRate_Tests tp
            , swapOffer_Validator_Redeemer_UpdateSellRestrictions_Tests tp
            , swapOffer_Validator_Redeemer_UpdateMinADA_Tests tp
            , swapOffer_Validator_Redeemer_Deposit_Tests tp
            , swapOffer_Validator_Redeemer_Withdraw_Tests tp
            , swapOffer_Validator_Redeemer_SwapFTxADA_Tests tp
            , swapOffer_Validator_Redeemer_SwapADAxFT_Tests tp
            , swapOffer_Validator_Redeemer_Delete_Tests tp
            ]

--------------------------------------------------------------------------------

swapOffer_Validator_Redeemer_UpdateStatus_Tests :: TestParams -> Tasty.TestTree
swapOffer_Validator_Redeemer_UpdateStatus_Tests tp =
    let
        ------------------------
        txName = show SwapOffer_UpdateStatus_Tx
        selectedRedeemer = RedeemerLogValidator (Just SwapOffer_UpdateStatus_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = swapOffer_UpdateStatus_TxContext tp T.swapOffer_Status_Open
                in
                    [
                    ]

--------------------------------------------------------------------------------

swapOffer_Validator_Redeemer_UpdateAskedCommissionRate_Tests :: TestParams -> Tasty.TestTree
swapOffer_Validator_Redeemer_UpdateAskedCommissionRate_Tests tp =
    let
        ------------------------
        txName = show SwapOffer_UpdateAskedCommissionRate_Tx
        selectedRedeemer = RedeemerLogValidator (Just SwapOffer_UpdateAskedCommissionRate_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = swapOffer_UpdateAskedCommissionRate_TxContext tp (ProtocolT.mmdMin $ ProtocolT.pdCommissionSwapOffer_InBPx1e3 $ protocol_DatumType_MockData tp)
                in
                    [
                    ]

--------------------------------------------------------------------------------

swapOffer_Validator_Redeemer_UpdateSellRestrictions_Tests :: TestParams -> Tasty.TestTree
swapOffer_Validator_Redeemer_UpdateSellRestrictions_Tests tp =
    let
        ------------------------
        txName = show SwapOffer_UpdateSellRestrictions_Tx
        selectedRedeemer = RedeemerLogValidator (Just SwapOffer_UpdateSellRestrictions_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = swapOffer_UpdateSellRestrictions_TxContext tp T.swapOffer_NotAllowSell T.swapOffer_NotAllowSell
                in
                    [
                    ]

--------------------------------------------------------------------------------

swapOffer_Validator_Redeemer_UpdateMinADA_Tests :: TestParams -> Tasty.TestTree
swapOffer_Validator_Redeemer_UpdateMinADA_Tests tp =
    let
        ------------------------
        txName = show SwapOffer_UpdateMinADA_Tx
        selectedRedeemer = RedeemerLogValidator (Just SwapOffer_UpdateMinADA_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = swapOffer_UpdateMinADA_TxContext tp toAlter_minAda
                in
                    [
                    ]

--------------------------------------------------------------------------------

swapOffer_Validator_Redeemer_Deposit_Tests :: TestParams -> Tasty.TestTree
swapOffer_Validator_Redeemer_Deposit_Tests tp =
    let
        ------------------------
        txName = show SwapOffer_Deposit_Tx
        selectedRedeemer = RedeemerLogValidator (Just SwapOffer_Deposit_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = swapOffer_Deposit_TxContext tp
                in
                    [
                    ]

--------------------------------------------------------------------------------

swapOffer_Validator_Redeemer_Withdraw_Tests :: TestParams -> Tasty.TestTree
swapOffer_Validator_Redeemer_Withdraw_Tests tp =
    let
        ------------------------
        txName = show SwapOffer_Withdraw_Tx
        selectedRedeemer = RedeemerLogValidator (Just SwapOffer_Withdraw_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = swapOffer_Withdraw_TxContext tp
                in
                    [
                    ]

--------------------------------------------------------------------------------

swapOffer_Validator_Redeemer_SwapFTxADA_Tests :: TestParams -> Tasty.TestTree
swapOffer_Validator_Redeemer_SwapFTxADA_Tests tp =
    let
        ------------------------
        txName = show SwapOffer_SwapFTxADA_Tx
        selectedRedeemer = RedeemerLogValidator (Just SwapOffer_SwapFTxADA_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = swapOffer_SwapFTxADA_TxContext tp
                in
                    [
                    ]

--------------------------------------------------------------------------------

swapOffer_Validator_Redeemer_SwapADAxFT_Tests :: TestParams -> Tasty.TestTree
swapOffer_Validator_Redeemer_SwapADAxFT_Tests tp =
    let
        ------------------------
        txName = show SwapOffer_SwapADAxFT_Tx
        selectedRedeemer = RedeemerLogValidator (Just SwapOffer_SwapADAxFT_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = swapOffer_SwapADAxFT_TxContext tp
                in
                    [
                    ]

--------------------------------------------------------------------------------

swapOffer_Validator_Redeemer_Delete_Tests :: TestParams -> Tasty.TestTree
swapOffer_Validator_Redeemer_Delete_Tests tp =
    let
        ------------------------
        txName = show SwapOffer_Delete_Tx
        selectedRedeemer = RedeemerLogValidator (Just SwapOffer_Delete_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = swapOffer_Delete_TxContext tp
                in
                    [
                    ]

--------------------------------------------------------------------------------

