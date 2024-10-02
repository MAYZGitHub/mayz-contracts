--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

{- |
Module      : Tests.PropertyBasedTests.Fund.Holding.MintingPolicy
Description : Validation logic and property-based test cases related to the
              FundHolding minting policy.

This module defines the validation logic for the FundHolding's minting policy.

It includes multiple test cases to ensure the integrity and correctness of the
minting script.
-}
module Contracts.Fund.Holding.MintingPolicy
    ( fundHolding_Policy_Tests
    ) where

-- Non-IOG imports
import qualified Control.Monad.Reader                      as MReader
import           Prelude
import qualified Test.QuickCheck                           as QC
import qualified Test.Tasty                                as Tasty
import qualified Test.Tasty.QuickCheck                     as TastyQC

-- IOG imports
import qualified Plutus.V2.Ledger.Api                      as LedgerApiV2
import           PlutusTx.Prelude                          ()

-- Project imports
import qualified Protocol.Fund.Holding.Types               as FundHoldingT
import           TestUtils.Contracts.TxContext.FundHolding
import           TestUtils.HelpersMAYZ
import           TestUtils.TestContext.Asserts
import           TestUtils.TestContext.Helpers
import           TestUtils.Types
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------

fundHolding_Policy_Tests :: AppM Tasty.TestTree
fundHolding_Policy_Tests = do
    tp <- MReader.asks appTestParams
    return $
        Tasty.testGroup
        "FundHolding Policy Tests"
        [
            fundHolding_Policy_Redeemer_MintID_Tests tp,
            fundHolding_Policy_Redeemer_BurnID_Tests tp
        ]

--------------------------------------------------------------------------------
-- MINTING FUND HOLDING ID TOKEN
--------------------------------------------------------------------------------

fundHolding_Policy_Redeemer_MintID_Tests :: TestParams -> Tasty.TestTree
fundHolding_Policy_Redeemer_MintID_Tests tp =
    let
        ------------------------
        txName = show FundHolding_Create_Tx
        selectedRedeemer = RedeemerLogPolicy (Just FundHolding_MintID_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = fundHolding_Create_TxContext tp
                in
                    [
                        TastyQC.testProperty
                        "Minting an invalid amount of FundHoldingID NFT must fail"
                        (prop_mintInvalidAmount tp selectedRedeemer ctx)
                    ]


--------------------------------------------------------------------------------
-- BURNING FUND HOLDING ID TOKEN
--------------------------------------------------------------------------------

fundHolding_Policy_Redeemer_BurnID_Tests :: TestParams -> Tasty.TestTree
fundHolding_Policy_Redeemer_BurnID_Tests tp =
    let
        ------------------------
        txName = show FundHolding_Delete_Tx
        selectedRedeemer = RedeemerLogPolicy (Just FundHolding_BurnID_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = fundHolding_Delete_TxContext tp
                in
                    [
                        TastyQC.testProperty
                        "Burning an invalid amount of FundHoldingID NFT must fail"
                        (prop_burnInvalidAmount tp selectedRedeemer ctx)
                    ]

--------------------------------------------------------------------------------

-- | Minting an invalid amount of FundHoldingID NFTs.
prop_mintInvalidAmount :: TestParams -> RedeemerLog -> LedgerApiV2.ScriptContext -> QC.Property
prop_mintInvalidAmount tp selectedRedeemer ctx = QC.forAll nonOneNorZeroInteger $
    \randomInteger -> do
        let
            ctx' = ctx
                    |> setMintAndAddRedeemers
                        [( LedgerApiV2.singleton
                            (tpFundHoldingPolicyID_CS tp)
                            (mkFundHoldingID_TN 0)
                            randomInteger
                        , FundHoldingT.mkMintIDRedeemer)]
        results <- testContextWrapper tp ctx'
        (Just selectedRedeemer, results)
            `assertResultsContainAnyOf` ["not isMintingFundHoldingID"]
    where
        nonOneNorZeroInteger :: QC.Gen Integer
        nonOneNorZeroInteger = QC.arbitrary `QC.suchThat` \x -> x /= 1 && x /= 0

--------------------------------------------------------------------------------

-- | Burning an invalid amount of FundHoldingID NFTs.
prop_burnInvalidAmount :: TestParams -> RedeemerLog -> LedgerApiV2.ScriptContext -> QC.Property
prop_burnInvalidAmount tp selectedRedeemer ctx = QC.forAll nonOneNorZeroInteger $
    \randomInteger -> do
        let
            ctx' = ctx
                    |> setMintAndAddRedeemers
                        [( LedgerApiV2.singleton
                            (tpFundHoldingPolicyID_CS tp)
                            (mkFundHoldingID_TN 0)
                            (negate randomInteger)
                         , FundHoldingT.mkBurnIDRedeemer)]
        results <- testContextWrapper tp ctx'
        (Just selectedRedeemer, results)
            `assertResultsContainAnyOf` ["not isBurningFundHoldingID"]
    where
        nonOneNorZeroInteger :: QC.Gen Integer
        nonOneNorZeroInteger = QC.arbitrary `QC.suchThat` \x -> x /= 1 && x /= 0

--------------------------------------------------------------------------------
