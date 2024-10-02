--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4
{- |
Module      : SellOffer.MintingPolicy
Description : Validation logic and tests related to the SellOffer minting policy.

This module defines the validation logic for the SellOffer's minting policy.

It includes multiple test cases to ensure the integrity and correctness of the
minting script.
-}
module Contracts.SellOffer.MintingPolicy where
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
-- import qualified Protocol.SellOffer.Types                as SellOfferT
-- import           TestUtils.Contracts.InitialData
import           TestUtils.Contracts.TxContext.SellOffer
-- import           TestUtils.HelpersMAYZ
-- import           TestUtils.TestContext.Asserts
-- import           TestUtils.TestContext.Helpers
-- import           TestUtils.Types
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------

sellOffer_Policy_Tests :: AppM Tasty.TestTree
sellOffer_Policy_Tests = do
    tp <- MReader.asks appTestParams
    return $
        Tasty.testGroup
        "SellOffer Policy Tests"
        [
            sellOffer_Policy_Redeemer_MintID_Tests tp,
            sellOffer_Policy_Redeemer_BurnID_Tests tp
        ]

--------------------------------------------------------------------------------

sellOffer_Policy_Redeemer_MintID_Tests :: TestParams -> Tasty.TestTree
sellOffer_Policy_Redeemer_MintID_Tests tp =
    let
        ------------------------
        txName = show SellOffer_Create_Tx
        selectedRedeemer = RedeemerLogPolicy (Just SellOffer_MintID_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = sellOffer_Create_TxContext tp
                in
                    [
                    ]


--------------------------------------------------------------------------------

sellOffer_Policy_Redeemer_BurnID_Tests :: TestParams -> Tasty.TestTree
sellOffer_Policy_Redeemer_BurnID_Tests tp =
    let
        ------------------------
        txName = show SellOffer_Delete_Tx
        selectedRedeemer = RedeemerLogPolicy (Just SellOffer_BurnID_TestRedeemer)
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
