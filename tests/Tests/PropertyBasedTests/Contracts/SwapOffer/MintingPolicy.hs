--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4
{- |
Module      : SwapOffer.MintingPolicy
Description : Validation logic and tests related to the SwapOffer minting policy.

This module defines the validation logic for the SwapOffer's minting policy.

It includes multiple test cases to ensure the integrity and correctness of the
minting script.
-}
module Contracts.SwapOffer.MintingPolicy where
--------------------------------------------------------------------------------
-- Non-IOG imports
import qualified Control.Monad.Reader                    as MReader
import           Prelude                                 (show)
import qualified Test.Tasty                              as Tasty
-- import qualified Test.Tasty.HUnit                        as Tasty
-- IOG imports
import           PlutusTx.Prelude

-- Project imports
-- import qualified Plutus.V2.Ledger.Api                    as LedgerApiV2
-- import qualified Protocol.Protocol.Types                 as ProtocolT
-- import qualified Protocol.SwapOffer.Types                as SwapOfferT
-- import           TestUtils.Contracts.InitialData
import           TestUtils.Contracts.TxContext.SwapOffer
-- import           TestUtils.HelpersMAYZ
-- import           TestUtils.TestContext.Asserts
-- import           TestUtils.TestContext.Helpers
-- import           TestUtils.Types
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------

swapOffer_Policy_Tests :: AppM Tasty.TestTree
swapOffer_Policy_Tests = do
    tp <- MReader.asks appTestParams
    return $
        Tasty.testGroup
        "SwapOffer Policy Tests"
        [
            swapOffer_Policy_Redeemer_MintID_Tests tp,
            swapOffer_Policy_Redeemer_BurnID_Tests tp
        ]

--------------------------------------------------------------------------------

swapOffer_Policy_Redeemer_MintID_Tests :: TestParams -> Tasty.TestTree
swapOffer_Policy_Redeemer_MintID_Tests tp =
    let
        ------------------------
        txName = show SwapOffer_Create_Tx
        selectedRedeemer = RedeemerLogPolicy (Just SwapOffer_MintID_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = swapOffer_Create_TxContext tp
                in
                    [
                    ]


--------------------------------------------------------------------------------

swapOffer_Policy_Redeemer_BurnID_Tests :: TestParams -> Tasty.TestTree
swapOffer_Policy_Redeemer_BurnID_Tests tp =
    let
        ------------------------
        txName = show SwapOffer_Delete_Tx
        selectedRedeemer = RedeemerLogPolicy (Just SwapOffer_BurnID_TestRedeemer)
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
