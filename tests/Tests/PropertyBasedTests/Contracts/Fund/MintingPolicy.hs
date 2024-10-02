--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

{- |
Module      : Tests.PropertyBasedTests.Fund.MintingPolicy
Description : Validation logic and property-based tests related to the Fund
              minting policy.

This module defines the validation logic for the Fund Minting Policy.

It includes multiple property-based test cases to ensure the integrity and
correctness of the minting policy.
-}
module Contracts.Fund.MintingPolicy where

--------------------------------------------------------------------------------

-- Non-IOG imports
import qualified Control.Monad.Reader                  as MReader
import           Prelude
import qualified Test.QuickCheck                       as QC
import qualified Test.Tasty                            as Tasty
import qualified Test.Tasty.QuickCheck                 as TastyQC

-- IOG imports
import qualified Ledger.Ada                            as LedgerAda
import qualified Plutus.V2.Ledger.Api                  as LedgerApiV2
import           PlutusTx.Prelude                      ()

-- Project imports
import qualified Protocol.Constants                    as T
import qualified Protocol.Fund.Types                   as FundT
import           TestUtils.Contracts.InitialData
import           TestUtils.Contracts.TxContext.Fund
import           TestUtils.HelpersMAYZ
import           TestUtils.QuickCheckGen.QuickCheckGen
import           TestUtils.TestContext.Asserts
import           TestUtils.TestContext.Helpers
import           TestUtils.Types
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------

fund_Policy_Tests :: AppM Tasty.TestTree
fund_Policy_Tests = do
    tp <- MReader.asks appTestParams
    return $
        Tasty.testGroup
        "Fund Policy Tests"
        [
             fund_Policy_Redeemer_MintID_Tests tp
            , fund_Policy_Redeemer_BurnID_Tests tp
            , fund_Policy_Redeemer_MintFT_Tests tp
            , fund_Policy_Redeemer_BurnFT_Tests tp
        ]

--------------------------------------------------------------------------------

fund_Policy_Redeemer_MintID_Tests :: TestParams -> Tasty.TestTree
fund_Policy_Redeemer_MintID_Tests tp =
    let
        ------------------------
        txName = show Fund_Create_Tx
        selectedRedeemer = RedeemerLogPolicy (Just Fund_MintID_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = fund_Create_TxContext tp
                in
                    [
                        TastyQC.testProperty
                            "Minting an invalid amount of FundID NFT must fail"
                            (prop_mintOneFundID tp selectedRedeemer ctx)
                        , TastyQC.testProperty
                            "Minting an invalid amount of InvestUnitID NFT must fail"
                            (prop_mintOneInvestUnitID tp selectedRedeemer ctx)
                        , TastyQC.testProperty
                            "Different Fund UTxO Address from the one in Policy Param must fail"
                            (prop_fundUTXOAddressDifferent tp selectedRedeemer ctx)
                        , TastyQC.testProperty
                            "Having the initial holdings count different from 0 must fail"
                            (prop_initialHoldingsCountNonZero tp selectedRedeemer ctx)
                        , TastyQC.testProperty
                            "Paying trash tokens to the Fund UTxO must fail"
                            (prop_payingTrashTokens tp selectedRedeemer ctx)
                        , TastyQC.testProperty
                            "Paying incorrect amount of MAYZ Tokens to the Fund UTxO must fail"
                            (prop_payingIncorrectMayzAmount tp selectedRedeemer ctx)
                        , TastyQC.testProperty
                            "Not paying correct minADA to InvestUnit UTxO must fail"
                            (prop_minAdaInvestUnitUTxO tp selectedRedeemer ctx)
                    ]

--------------------------------------------------------------------------------

fund_Policy_Redeemer_BurnID_Tests :: TestParams -> Tasty.TestTree
fund_Policy_Redeemer_BurnID_Tests tp =
    let
        ------------------------
        txName = show Fund_Delete_Tx
        selectedRedeemer = RedeemerLogPolicy (Just Fund_BurnID_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = fund_Delete_TxContext tp
                in
                    [
                        TastyQC.testProperty
                            "Burning an invalid amount of FundID NFT must fail"
                            (prop_burnOneFundID tp selectedRedeemer ctx)
                        , TastyQC.testProperty
                            "Burning an invalid amount of InvestUnitID NFT must fail"
                            (prop_burnOneInvestUnitID tp selectedRedeemer ctx)
                    ]

fund_Policy_Redeemer_MintFT_Tests :: TestParams -> Tasty.TestTree
fund_Policy_Redeemer_MintFT_Tests tp =
    let
        ------------------------
        txName = show Fund_Deposit_Tx
        selectedRedeemer = RedeemerLogPolicy (Just Fund_MintFT_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = fund_Deposit_TxContext tp (tpDepositDate tp) deposit_MockData
                in
                    [
                       --TODO
                    ]

--------------------------------------------------------------------------------

fund_Policy_Redeemer_BurnFT_Tests :: TestParams -> Tasty.TestTree
fund_Policy_Redeemer_BurnFT_Tests tp =
    let
        ------------------------
        txName = show Fund_Withdraw_Tx
        selectedRedeemer = RedeemerLogPolicy (Just Fund_MintFT_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = fund_Withdraw_TxContext tp (tpDepositDate tp) deposit_MockData(tpWithdrawDate tp) withdraw_MockData
                in
                    [
                       --TODO
                    ]

--------------------------------------------------------------------------------


-- | Minting an invalid amount of a specific NFT.
prop_mintInvalidAmount :: TestParams -> RedeemerLog -> LedgerApiV2.ScriptContext -> LedgerApiV2.TokenName -> LedgerApiV2.TokenName -> QC.Property
prop_mintInvalidAmount tp selectedRedeemer ctx firstTN secondTN = QC.forAll nonOneInteger $
    \randomInteger -> do
        let ctx' = ctx
                    |> setMintAndAddRedeemers
                        [
                            ( LedgerApiV2.singleton (tpFundPolicy_CS tp) firstTN randomInteger
                                <> LedgerApiV2.singleton (tpFundPolicy_CS tp) secondTN 1
                            , FundT.mkMintIDRedeemer
                            )
                        ]
        results <- testContextWrapper tp ctx'
        (Just selectedRedeemer, results) `assertResultsContainAnyOf` ["not isMintingIDs"]
    where
        nonOneInteger :: QC.Gen Integer
        nonOneInteger = QC.arbitrary `QC.suchThat` \x -> x /= 1

-- | Minting an invalid amount of FundID NFT must fail.
prop_mintOneFundID :: TestParams -> RedeemerLog -> LedgerApiV2.ScriptContext -> QC.Property
prop_mintOneFundID tp selectedRedeemer ctx = prop_mintInvalidAmount tp selectedRedeemer ctx T.fundID_TN T.investUnitID_TN

-- | Minting an invalid amount of InvestUnitID NFT must fail.
prop_mintOneInvestUnitID :: TestParams -> RedeemerLog -> LedgerApiV2.ScriptContext -> QC.Property
prop_mintOneInvestUnitID tp selectedRedeemer ctx = prop_mintInvalidAmount tp selectedRedeemer ctx T.investUnitID_TN T.fundID_TN

{- | Paying the FundID NFT to a different script address than the one that's in
     the PolicyParam must fail.
-}
prop_fundUTXOAddressDifferent :: TestParams -> RedeemerLog -> LedgerApiV2.ScriptContext -> LedgerApiV2.Address -> QC.Property
prop_fundUTXOAddressDifferent tp selectedRedeemer ctx randomAddress =
    QC.once $ do
        let fund_UTxO_MockData' = (fund_UTxO_MockData tp) {LedgerApiV2.txOutAddress = randomAddress}
            ctx' = ctx
                    |> setOutputs [fund_UTxO_MockData', investUnit_UTxO_MockData tp]
        results <- testContextWrapper tp ctx'
        (Just selectedRedeemer, results) `assertResultsContainAnyOf` ["Expected Fund at output index 0"]

-- | Having the initial holdings count different from 0 must fail.
prop_initialHoldingsCountNonZero :: TestParams -> RedeemerLog -> LedgerApiV2.ScriptContext -> QC.Property
prop_initialHoldingsCountNonZero tp selectedRedeemer ctx = QC.forAll nonZeroInteger $ \holdingsCount -> do
    let fund_DatumType' = (fund_DatumType_MockData tp) {FundT.fdHoldingsCount = holdingsCount}
        fund_UTxO_MockData' =
            (fund_UTxO_MockData tp)
                { LedgerApiV2.txOutDatum =
                    LedgerApiV2.OutputDatum $
                        FundT.mkDatum fund_DatumType'
                }
        ctx' = ctx
                    |> setOutputs [fund_UTxO_MockData', investUnit_UTxO_MockData tp]
    results <- testContextWrapper tp ctx'
    (Just selectedRedeemer, results) `assertResultsContainAnyOf` ["not isCorrect_Output_Fund_Datum"]
    where
        nonZeroInteger :: QC.Gen Integer
        nonZeroInteger = QC.arbitrary `QC.suchThat` \x -> x /= 0

-- | Paying trash tokens to the Fund UTxO must fail.
prop_payingTrashTokens :: TestParams -> RedeemerLog -> LedgerApiV2.ScriptContext -> RandomSingleton -> QC.Property
prop_payingTrashTokens tp selectedRedeemer ctx randomSingleton =
    QC.once $ do
        let fund_UTxO_MockData' =
                (fund_UTxO_MockData tp)
                    { LedgerApiV2.txOutValue =
                        LedgerAda.lovelaceValueOf minAdaFundDatum
                            <> LedgerApiV2.singleton (tpFundPolicy_CS tp) T.fundID_TN 1
                            <> LedgerApiV2.singleton (tpTokenMAYZ_CS tp) (tpTokenMAYZ_TN tp) 7
                            <> getRandomSingleton randomSingleton -- trash token
                    }
            ctx' = ctx
                    |> setOutputs [fund_UTxO_MockData', investUnit_UTxO_MockData tp]
        results <- testContextWrapper tp ctx'
        (Just selectedRedeemer, results) `assertResultsContainAnyOf` ["not isCorrect_Output_Fund_Value"]

-- | Paying incorrect amount of MAYZ Tokens to the Fund UTxO must fail.
prop_payingIncorrectMayzAmount :: TestParams -> RedeemerLog -> LedgerApiV2.ScriptContext -> QC.Property
prop_payingIncorrectMayzAmount tp selectedRedeemer ctx = QC.forAll nonSevenInteger $ \randomInteger -> do
    let fund_UTxO_MockData' =
            (fund_UTxO_MockData tp)
                { LedgerApiV2.txOutValue =
                    LedgerAda.lovelaceValueOf minAdaFundDatum
                        <> LedgerApiV2.singleton (tpFundPolicy_CS tp) T.fundID_TN 1
                        <> LedgerApiV2.singleton (tpTokenMAYZ_CS tp) (tpTokenMAYZ_TN tp) randomInteger
                }
        ctx' = ctx
                |> setOutputs [fund_UTxO_MockData', investUnit_UTxO_MockData tp]
    results <- testContextWrapper tp ctx'
    (Just selectedRedeemer, results) `assertResultsContainAnyOf` ["not isCorrect_Output_Fund_Value"]
    where
        nonSevenInteger :: QC.Gen Integer
        nonSevenInteger = QC.arbitrary `QC.suchThat` \x -> x /= 7

-- | Not paying correct minADA to InvestUnit UTxO must fail
prop_minAdaInvestUnitUTxO :: TestParams -> RedeemerLog -> LedgerApiV2.ScriptContext -> QC.Property
prop_minAdaInvestUnitUTxO tp selectedRedeemer ctx =
    QC.forAll nonMinADAInteger $ \randomInteger -> do
        let investUnitUTxO'' =
                (investUnit_UTxO_MockData tp)
                    { LedgerApiV2.txOutValue =
                        LedgerAda.lovelaceValueOf randomInteger
                            <> LedgerApiV2.singleton (tpFundPolicy_CS tp) T.investUnitID_TN 1
                    }
            ctx' = ctx
                    |> setOutputs [fund_UTxO_MockData tp, investUnitUTxO'']
        results <- testContextWrapper tp ctx'
        (Just selectedRedeemer, results) `assertResultsContainAnyOf` ["not isCorrect_Output_InvestUnit_Value"]
    where
        nonMinADAInteger :: QC.Gen Integer
        nonMinADAInteger = QC.arbitrary `QC.suchThat` \x -> x /= minAdaIUDatum


-- | Minting an invalid amount of a specific NFT.
prop_burnInvalidAmount :: TestParams -> RedeemerLog -> LedgerApiV2.ScriptContext -> LedgerApiV2.TokenName -> LedgerApiV2.TokenName -> QC.Property
prop_burnInvalidAmount tp selectedRedeemer ctx firstTN secondTN = QC.forAll nonNegativeOneInteger $
    \randomInteger -> do
        let ctx' = ctx
                    |> setMintAndAddRedeemers
                        [
                            ( LedgerApiV2.singleton (tpFundPolicy_CS tp) firstTN randomInteger
                                <> LedgerApiV2.singleton (tpFundPolicy_CS tp) secondTN (-1)
                            , FundT.mkBurnIDRedeemer
                            )
                        ]
        results <- testContextWrapper tp ctx'
        (Just selectedRedeemer, results) `assertResultsContainAnyOf` ["not isBurningIDs"]
    where
        nonNegativeOneInteger :: QC.Gen Integer
        nonNegativeOneInteger = QC.arbitrary `QC.suchThat` \x -> x /= -1

{- | Burning an invalid amount of FundID NFT, or trying to mint it instead,
   must fail.
-}
prop_burnOneFundID :: TestParams -> RedeemerLog -> LedgerApiV2.ScriptContext -> QC.Property
prop_burnOneFundID tp selectedRedeemer ctx = prop_burnInvalidAmount tp selectedRedeemer ctx T.fundID_TN T.investUnitID_TN

{- | Burning an invalid amount of InvestUnitID NFT, or trying to mint it
     instead, must fail.
-}
prop_burnOneInvestUnitID :: TestParams -> RedeemerLog -> LedgerApiV2.ScriptContext -> QC.Property
prop_burnOneInvestUnitID tp selectedRedeemer ctx = prop_burnInvalidAmount tp selectedRedeemer ctx T.investUnitID_TN T.fundID_TN
