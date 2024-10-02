--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4
{- |
Module      : Tests.PropertyBasedTests.Protocol.MintingPolicy
Description : Validation logic and property-based tests related to the Protocol
              minting policy.

This module defines the validation logic for the Protocol Minting Policy.

It includes multiple property-based test cases to ensure the integrity and
correctness of the minting policy.
-}
module Contracts.Protocol.MintingPolicy where
    
--------------------------------------------------------------------------------4

-- Non-IOG imports
import qualified Control.Monad.Reader                   as MReader
import           Prelude
import qualified Test.QuickCheck                        as QC
import qualified Test.Tasty                             as Tasty
import qualified Test.Tasty.QuickCheck                  as TastyQC

-- IOG imports
import qualified Plutus.V2.Ledger.Api                   as LedgerApiV2
import           PlutusTx.Prelude                       ()

-- Project imports
import           TestUtils.QuickCheckGen.QuickCheckGen
import qualified Protocol.Constants                     as T
import qualified Protocol.Protocol.Types                as ProtocolT
import           TestUtils.Contracts.TxContext.Protocol
import           TestUtils.HelpersMAYZ
import           TestUtils.TestContext.Asserts
import           TestUtils.TestContext.Helpers
import           TestUtils.Types
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------4

protocol_Policy_Tests :: AppM Tasty.TestTree
protocol_Policy_Tests = do
    tp <- MReader.asks appTestParams
    return $
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
                        TastyQC.testProperty
                            "Minting an invalid amount of ProtocolID NFT must fail"
                            (prop_mintedValue tp selectedRedeemer ctx)
                        , TastyQC.testProperty
                            "Spending a different UTxO must fail"
                            (prop_spentUTxO tp selectedRedeemer ctx)
                    ]

--------------------------------------------------------------------------------

-- | Tries to mint an invalid amount of ProtocolID NFTs.
prop_mintedValue :: TestParams -> RedeemerLog -> LedgerApiV2.ScriptContext -> QC.Property
prop_mintedValue tp selectedRedeemer ctx = QC.forAll nonOneNorZeroInteger $ \randomInteger ->  do
    let
        ctx' = ctx
                |> setMintAndAddRedeemers
                    [(LedgerApiV2.singleton (tpProtocolPolicyID_CS tp) T.protocolID_TN randomInteger,ProtocolT.mkMintIDRedeemer)]
    results <- testContextWrapper tp ctx'
    (Just selectedRedeemer, results)
            `assertResultsContainAnyOf` ["not isMintingID"]
    where
        nonOneNorZeroInteger :: QC.Gen Integer
        nonOneNorZeroInteger = QC.arbitrary `QC.suchThat` \x -> x /= 1 && x /= 0

{- | Tries to spend a different UTxO than the one that's specified in the
   Minting Policy Parameters.
-}
prop_spentUTxO :: TestParams -> RedeemerLog -> LedgerApiV2.ScriptContext -> LedgerApiV2.TxOut -> LedgerApiV2.TxOutRef -> QC.Property
prop_spentUTxO tp selectedRedeemer ctx randomTxOut randomTxOutRef = QC.once $ do
    let
        ctx' = ctx
                |> setInputWithTxOufRef (randomTxOut, randomTxOutRef)
    results <- testContextWrapper tp ctx'
    (Just selectedRedeemer, results)
            `assertResultsContainAnyOf` ["not isTxOutAnInput"]
            
--------------------------------------------------------------------------------
