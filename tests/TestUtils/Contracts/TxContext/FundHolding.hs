--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

{- |
Module      : TestUtils.Contracts.TxContext.FundHolding
Description :
-}
module TestUtils.Contracts.TxContext.FundHolding where

-- Non-IOG imports
import           Prelude                                  as P hiding (negate, (<>))

-- IOG imports
import qualified Plutus.V2.Ledger.Api                     as LedgerApiV2
import           PlutusTx.Prelude                         (negate, (<>))
import qualified PlutusTx.Ratio as TxRatio
import qualified PlutusTx.Prelude as Ptx

-- Project imports
import qualified Generic.OnChainHelpers                   as OnChainHelpers
import qualified Protocol.Fund.Helpers                    as FundHelpers
import qualified Protocol.Fund.Holding.Types              as FundHoldingT
import qualified Protocol.Fund.Types                      as FundT
import qualified Protocol.Fund.InvestUnit.Types                as InvestUnitT
import qualified Protocol.OffChainHelpers                 as OffChainHelpers
import qualified Protocol.OnChainHelpers                  as OnChainHelpers
import qualified Protocol.Types                           as T
import           TestUtils.Contracts.InitialData
import           TestUtils.Contracts.TxContext.InvestUnit
import           TestUtils.Helpers
import           TestUtils.HelpersMAYZ
import           TestUtils.TestContext.Helpers
import           TestUtils.Types
import           TestUtils.TypesMAYZ


--------------------------------------------------------------------------------
-- FundHolding Contract
--------------------------------------------------------------------------------

fundHolding_Create_TxContext :: TestParams -> LedgerApiV2.ScriptContext
fundHolding_Create_TxContext tp =
    mkContext
        |> setInputsRef [ uTxOForValidatorAsReference tp (tpFundValidator tp), uTxOForMintingAsReference tp (tpFundHoldingPolicyID tp)]
        |> setInputsAndAddRedeemers [(fund_UTxO_MockData tp, FundT.mkFundHoldingAddRedeemer)]
        |> setOutputs [fund_UTxO_With_Added_FundHolding_MockData tp, fundHolding_UTxO_With_NoDeposits_MockData tp]
        |> setMintAndAddRedeemers
                [(  LedgerApiV2.singleton
                            (tpFundHoldingPolicyID_CS tp)
                            (mkFundHoldingID_TN 0)
                            1
                        , FundHoldingT.mkMintIDRedeemer)]
        |> setSignatories (tpFundAdmins tp)
        |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------

fundHolding_Delete_TxContext :: TestParams -> LedgerApiV2.ScriptContext
fundHolding_Delete_TxContext tp =
    mkContext
        |> setInputsRef [ uTxOForValidatorAsReference tp (tpFundValidator tp), uTxOForValidatorAsReference tp (tpFundHoldingValidator tp), uTxOForMintingAsReference tp (tpFundHoldingPolicyID tp)]
        |> setInputsAndAddRedeemers [(fund_UTxO_With_Added_FundHolding_MockData tp, FundT.mkFundHoldingDeleteRedeemer), (fundHolding_UTxO_With_NoDeposits_MockData tp, FundHoldingT.mkDeleteRedeemer)]
        |> setOutputs [fund_UTxO_With_Deleted_FundHolding_MockData tp]
        |> setMintAndAddRedeemers
                [(  LedgerApiV2.singleton
                            (tpFundHoldingPolicyID_CS tp)
                            (mkFundHoldingID_TN 0)
                            (-1)
                        , FundHoldingT.mkBurnIDRedeemer)]
        |> setSignatories (tpFundAdmins tp)
        |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------

fundHolding_UpdateMinADA_TxContext :: TestParams -> Integer -> LedgerApiV2.ScriptContext
fundHolding_UpdateMinADA_TxContext tp newMinADA =
    let
        input_FundHolding_UTxO = fundHolding_UTxO_With_NoDeposits_MockData tp
        input_FundHolding_Datum = FundHoldingT.getFundHolding_DatumType_From_UTxO input_FundHolding_UTxO
        input_FundHolding_Value = LedgerApiV2.txOutValue input_FundHolding_UTxO
        -----------------
        output_FundHolding_Datum = FundHelpers.mkUpdated_FundHolding_Datum_With_MinADAChanged
                input_FundHolding_Datum
                newMinADA
        output_FundHolding_UTxO = input_FundHolding_UTxO
            { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $
                    FundHoldingT.mkDatum output_FundHolding_Datum
            , LedgerApiV2.txOutValue =
                changeValue_Amount
                    input_FundHolding_Value
                    OnChainHelpers.adaAssetClass
                    newMinADA
            }
        -----------------
    in
        mkContext
            |> setInputsRef [fund_UTxO_MockData tp]
            |> setInputsAndAddRedeemers [(input_FundHolding_UTxO, FundHoldingT.mkUpdateMinADARedeemer)]
            |> setOutputs [output_FundHolding_UTxO]
            |> setSignatories (tpFundAdmins tp)
            |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------

fundHolding_Deposit_TxContext_Wrapper :: TestParams -> TxContextParametrizable
fundHolding_Deposit_TxContext_Wrapper tp txParams =
    let
        depositDate = getTxParam "depositDate" txParams :: LedgerApiV2.POSIXTime
        depositAmount = getTxParam "depositAmount" txParams :: Integer
    in fundHolding_Deposit_TxContext tp depositDate depositAmount

fundHolding_Deposit_TxContext :: TestParams -> LedgerApiV2.POSIXTime -> Integer -> LedgerApiV2.ScriptContext
fundHolding_Deposit_TxContext tp depositDate depositAmount =
    let
        --------------------
        depositAmountSafe = if depositAmount < 1 then 1 else depositAmount
        --------------------
        input_Fund_UTxO = fund_UTxO_MockData tp
        input_Fund_Datum = FundT.getFund_DatumType_From_UTxO input_Fund_UTxO
        --------------------
        input_InvestUnit_UTxO = investUnit_UTxO_MockData tp
        input_InvestUnit_Datum = InvestUnitT.getInvestUnit_DatumType_From_UTxO input_InvestUnit_UTxO
        input_InvestUnit = InvestUnitT.iudInvestUnit input_InvestUnit_Datum
        --------------------
        input_FundHolding_UTxO = fundHolding_UTxO_With_NoDeposits_MockData tp
        input_FundHolding_Datum = FundHoldingT.getFundHolding_DatumType_From_UTxO input_FundHolding_UTxO
        input_FundHolding_Value = LedgerApiV2.txOutValue input_FundHolding_UTxO
        --------------------
        (userFT, commissionsFT, commissions_FT_Release_PerMonth_1e6) = calculateDepositCommissionsUsingMonths_Parametrizable tp input_Fund_Datum depositDate depositAmountSafe
        --------------------
        investUnit_Value = OffChainHelpers.mkValue_From_InvestUnit_And_Amount2 input_InvestUnit depositAmountSafe
        --------------------
        output_FundHolding_Datum = FundHelpers.mkUpdated_FundHolding_Datum_With_Deposit
                input_FundHolding_Datum
                depositAmountSafe userFT commissionsFT commissions_FT_Release_PerMonth_1e6
        output_FundHolding_UTxO = input_FundHolding_UTxO
            { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $
                    FundHoldingT.mkDatum output_FundHolding_Datum
            , LedgerApiV2.txOutValue = input_FundHolding_Value <> investUnit_Value <> LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) commissionsFT
            }
        -----------------
    in
        mkContext
            |> setInputsRef [input_Fund_UTxO, input_InvestUnit_UTxO,
                uTxOForValidatorAsReference tp (tpFundHoldingValidator tp), uTxOForMintingAsReference tp (tpFundPolicy tp)]
            |> setInputsAndAddRedeemers [(input_FundHolding_UTxO, FundHoldingT.mkDepositRedeemer depositDate depositAmount)]
            |> setOutputs [output_FundHolding_UTxO]
            |> setMintAndAddRedeemers[ (LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) depositAmountSafe, FundT.mkMintFTRedeemer)]
            |> setValidyRange (createValidRange depositDate)

--------------------------------------------------------------------------------

fundHolding_Withdraw_TxContext_Wrapper :: TestParams -> TxContextParametrizable
fundHolding_Withdraw_TxContext_Wrapper tp txParams =
    let
        depositDate = getTxParam "depositDate" txParams :: LedgerApiV2.POSIXTime
        depositAmount = getTxParam "depositAmount" txParams :: Integer
        withdrawDate = getTxParam "withdrawDate" txParams :: LedgerApiV2.POSIXTime
        withdrawAmount = getTxParam "withdrawAmount" txParams :: Integer
    in fundHolding_Withdraw_TxContext tp depositDate depositAmount withdrawDate withdrawAmount

fundHolding_Withdraw_TxContext :: TestParams -> LedgerApiV2.POSIXTime -> Integer -> LedgerApiV2.POSIXTime -> Integer -> LedgerApiV2.ScriptContext
fundHolding_Withdraw_TxContext tp depositDate depositAmount withdrawDate withdrawAmount =
    let
         --------------------
        withdrawAmountSafe = if withdrawAmount < 1 then 1 else withdrawAmount
        --------------------
        input_Fund_UTxO = fund_UTxO_MockData tp
        input_Fund_Datum = FundT.getFund_DatumType_From_UTxO input_Fund_UTxO
        --------------------
        input_InvestUnit_UTxO = investUnit_UTxO_MockData tp
        input_InvestUnit_Datum = InvestUnitT.getInvestUnit_DatumType_From_UTxO input_InvestUnit_UTxO
        input_InvestUnit = InvestUnitT.iudInvestUnit input_InvestUnit_Datum
        --------------------
        base_FundHolding_UTxO = fundHolding_UTxO_With_NoDeposits_MockData tp
        base_FundHolding_Datum = FundHoldingT.getFundHolding_DatumType_From_UTxO base_FundHolding_UTxO
        --------------------
        input_FundHolding_UTxO = fundHolding_UTxO_With_Deposits_MockData_Parametrizable tp input_Fund_Datum base_FundHolding_Datum input_InvestUnit 0 depositAmount depositDate
        input_FundHolding_Datum = FundHoldingT.getFundHolding_DatumType_From_UTxO input_FundHolding_UTxO
        input_FundHolding_Value = LedgerApiV2.txOutValue input_FundHolding_UTxO
        --------------------
        investUnit_Granularity = OnChainHelpers.getDecimalsInInvestUnit (T.iuValues input_InvestUnit)
        (commissionsForUserFTToGetBack, withdrawPlusCommissionsGetBack, commissions_FT_Release_PerMonth_1e6) = calculateWithdrawCommissionsUsingMonths_Parametrizable tp input_Fund_Datum withdrawDate withdrawAmountSafe investUnit_Granularity
        --------------------
        investUnit_Value = OffChainHelpers.mkValue_From_InvestUnit_And_Amount2 input_InvestUnit withdrawPlusCommissionsGetBack
        --------------------
        output_FundHolding_Datum = FundHelpers.mkUpdated_FundHolding_Datum_With_Withdraw
                input_FundHolding_Datum
                withdrawAmountSafe commissionsForUserFTToGetBack commissions_FT_Release_PerMonth_1e6
        output_FundHolding_UTxO = input_FundHolding_UTxO
            { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $
                    FundHoldingT.mkDatum output_FundHolding_Datum
            , LedgerApiV2.txOutValue = input_FundHolding_Value <> negate investUnit_Value <> negate (LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) commissionsForUserFTToGetBack)
            }
        -----------------
    in
        mkContext
            |> setInputsRef [input_Fund_UTxO, input_InvestUnit_UTxO,
                              uTxOForValidatorAsReference tp (tpFundHoldingValidator tp), uTxOForMintingAsReference tp (tpFundPolicy tp)]
            |> setInputsAndAddRedeemers [(input_FundHolding_UTxO, FundHoldingT.mkWithdrawRedeemer withdrawDate withdrawAmount withdrawPlusCommissionsGetBack)]
            |> setOutputs [output_FundHolding_UTxO]
            |> setMintAndAddRedeemers
                [( LedgerApiV2.singleton
                    (tpFundPolicy_CS tp)
                    (tpFundFT_TN tp)
                    (-withdrawPlusCommissionsGetBack)
                , FundT.mkBurnFTRedeemer)]
            |> setValidyRange (createValidRange withdrawDate)

--------------------------------------------------------------------------------

fundHolding_Collect_Protocol_Commission_TxContext :: TestParams -> LedgerApiV2.ScriptContext
fundHolding_Collect_Protocol_Commission_TxContext tp =
     mkContext
        |> setInputsRef [protocol_UTxO_MockData tp, fund_UTxO_MockData tp]
        |> setInputsAndAddRedeemers [(fundHolding_UTxO_With_Deposits_MockData tp, FundHoldingT.mkCollect_Protocol_CommissionRedeemer (tpCollectCommissionsDate tp) withdraw_Commissions_MockData)]
        |> setOutputs [fundHolding_UTxO_With_Collected_Protocol tp withdraw_Commissions_MockData]
        |> setSignatories (tpProtocolAdmins tp)
        |> setValidyRange (createValidRange (tpCollectCommissionsDate tp))

--------------------------------------------------------------------------------

fundHolding_Collect_Managers_Commission_TxContext :: TestParams -> LedgerApiV2.ScriptContext
fundHolding_Collect_Managers_Commission_TxContext tp =
 mkContext
        |> setInputsRef [protocol_UTxO_MockData tp, fund_UTxO_MockData tp]
        |> setInputsAndAddRedeemers [(fundHolding_UTxO_With_Deposits_MockData tp, FundHoldingT.mkCollect_Managers_CommissionRedeemer (tpCollectCommissionsDate tp) withdraw_Commissions_MockData)]
        |> setOutputs [fundHolding_UTxO_With_Collected_Managers tp withdraw_Commissions_MockData]
        |> setSignatories (tpFundAdmins tp)
        |> setValidyRange (createValidRange (tpCollectCommissionsDate tp))

--------------------------------------------------------------------------------

fundHolding_Collect_Delegators_Commission_TxContext :: TestParams -> LedgerApiV2.ScriptContext
fundHolding_Collect_Delegators_Commission_TxContext tp =
    mkContext
        |> setInputsRef [protocol_UTxO_MockData tp, fund_UTxO_MockData tp]
        |> setInputsAndAddRedeemers [(fundHolding_UTxO_With_Deposits_MockData tp, FundHoldingT.mkCollect_Delegators_CommissionRedeemer (tpCollectCommissionsDate tp) withdraw_Commissions_MockData)]
        |> setOutputs [fundHolding_UTxO_With_Collected_Delegators tp withdraw_Commissions_MockData]
        |> setSignatories (tpDelegatorsAdmins tp)
        |> setValidyRange (createValidRange (tpCollectCommissionsDate tp))

--------------------------------------------------------------------------------

fundHolding_ReIndexing_TxContext :: TestParams -> LedgerApiV2.ScriptContext
fundHolding_ReIndexing_TxContext  = investUnit_ReIndexing_TxContext

--------------------------------------------------------------------------------

fundHolding_BalanceAssets_TxContext :: TestParams -> [Integer] -> [Integer] -> [Integer] -> Bool -> [Integer] -> LedgerApiV2.ScriptContext
fundHolding_BalanceAssets_TxContext tp depositsInit depositsAlterations commissionsFTAlterationsForRedeemer useCustomRelease customReleases =
    let
        --------------------
        swTrace = False
        --------------------
        depositsInit_1 = P.head depositsInit
        depositsInit_2 = P.head (tail depositsInit)
        --------------------
        depositsAlter_1 = P.head depositsAlterations
        depositsAlter_2 = P.head (tail depositsAlterations)
        --------------------
        input_Fund_UTxO = fund_UTxO_MockData tp
        input_Fund_Datum = FundT.getFund_DatumType_From_UTxO input_Fund_UTxO
        --------------------
        input_InvestUnit_UTxO = investUnit_UTxO_MockData tp
        input_InvestUnit_Datum = InvestUnitT.getInvestUnit_DatumType_From_UTxO input_InvestUnit_UTxO
        --------------------
        input_InvestUnit = InvestUnitT.iudInvestUnit input_InvestUnit_Datum
        --------------------
        base_FundHolding_UTxO = fundHolding_UTxO_With_NoDeposits_MockData tp
        base_FundHolding_Datum = FundHoldingT.getFundHolding_DatumType_From_UTxO base_FundHolding_UTxO
        --------------------
        input_FundHolding1_UTxO = fundHolding_UTxO_With_Deposits_MockData_Parametrizable tp input_Fund_Datum base_FundHolding_Datum input_InvestUnit 0 depositsInit_1 (tpDepositDate tp)
        input_FundHolding1_Value = LedgerApiV2.txOutValue input_FundHolding1_UTxO
        -----------------
        input_FundHolding2_UTxO = fundHolding_UTxO_With_Deposits_MockData_Parametrizable tp input_Fund_Datum base_FundHolding_Datum input_InvestUnit 1 depositsInit_2 (tpDepositDate tp)
        input_FundHolding2_Value = LedgerApiV2.txOutValue input_FundHolding2_UTxO
        -----------------
        tokens_InvestUnit_ValueToAlter1 = OffChainHelpers.mkValue_From_InvestUnit_And_Amount2 input_InvestUnit depositsAlter_1
        tokens_InvestUnit_ValueToAlter2 = OffChainHelpers.mkValue_From_InvestUnit_And_Amount2 input_InvestUnit depositsAlter_2
        -----------------
        output_FundHolding1_Value = input_FundHolding1_Value <> tokens_InvestUnit_ValueToAlter1
        output_FundHolding2_Value = input_FundHolding2_Value <> tokens_InvestUnit_ValueToAlter2
        -----------------
        output_FundHolding1_UTxO = 
            if null commissionsFTAlterationsForRedeemer then
                input_FundHolding1_UTxO {
                        LedgerApiV2.txOutValue = output_FundHolding1_Value
                    }
            else
                let    
                    ------------------
                    datumIn1 = FundHoldingT.getFundHolding_DatumType_From_UTxO input_FundHolding1_UTxO
                    commissionChange1 = P.head commissionsFTAlterationsForRedeemer
                    ------------------
                    !oldCommissions1 = FundHoldingT.hdSubtotal_FT_Commissions datumIn1
                    !oldRelease1 = FundHoldingT.hdSubtotal_FT_Commissions_Release_PerMonth_1e6 datumIn1
                    ------------------
                    (newCommissions1, newRelease1)
                      | useCustomRelease = 
                          let
                            !newCommissions = oldCommissions1 + commissionChange1
                            !newRelease = P.head customReleases -- Tomar el primer valor de customReleases
                          in
                            (newCommissions, newRelease)
                      | oldCommissions1 == 0 =
                          let
                            !newCommissions = oldCommissions1 + commissionChange1
                            !newRelease = 0
                          in
                            (newCommissions, newRelease)
                      | otherwise =
                          let
                            !newCommissions = oldCommissions1 + commissionChange1
                            !changeRatio1 = TxRatio.unsafeRatio commissionChange1 oldCommissions1
                            !changeRelease1 = TxRatio.truncate (changeRatio1 Ptx.* TxRatio.fromInteger oldRelease1)
                            !newRelease = oldRelease1 + changeRelease1
                          in
                            (newCommissions, newRelease)
                in
                    input_FundHolding1_UTxO { 
                        LedgerApiV2.txOutDatum =
                                LedgerApiV2.OutputDatum $ FundHoldingT.mkDatum datumIn1 
                                {
                                    FundHoldingT.hdSubtotal_FT_Commissions = newCommissions1
                                    , FundHoldingT.hdSubtotal_FT_Commissions_Release_PerMonth_1e6 = newRelease1
                                }
                        , LedgerApiV2.txOutValue = output_FundHolding1_Value <> LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) commissionChange1
                    }
        -----------------
        output_FundHolding2_UTxO = 
            if length commissionsFTAlterationsForRedeemer < 2 then
                input_FundHolding2_UTxO{
                        LedgerApiV2.txOutValue = output_FundHolding2_Value
                    }
            else
                let    
                    ------------------
                    datumIn1 = FundHoldingT.getFundHolding_DatumType_From_UTxO input_FundHolding1_UTxO
                    commissionChange1 = P.head commissionsFTAlterationsForRedeemer
                    ------------------
                    datumIn2 = FundHoldingT.getFundHolding_DatumType_From_UTxO input_FundHolding2_UTxO
                    commissionChange2 = P.head (P.tail commissionsFTAlterationsForRedeemer)
                    ------------------
                    !oldCommissions1 = FundHoldingT.hdSubtotal_FT_Commissions datumIn1
                    !oldRelease1 = FundHoldingT.hdSubtotal_FT_Commissions_Release_PerMonth_1e6 datumIn1
                    ------------------
                    !oldCommissions2 = FundHoldingT.hdSubtotal_FT_Commissions datumIn2
                    !oldRelease2 = FundHoldingT.hdSubtotal_FT_Commissions_Release_PerMonth_1e6 datumIn2
                    ------------------
                    (newCommissions2, newRelease2)
                      | useCustomRelease = 
                          let
                            !newCommissions = oldCommissions2 + commissionChange2
                            !newRelease = P.head (P.tail customReleases) -- Tomar el segundo valor de customReleases
                          in
                            (newCommissions, newRelease)
                      | oldCommissions1 == 0 =
                          let
                            !newCommissions = oldCommissions2 + commissionChange2
                            !newRelease = 0
                          in
                            (newCommissions, newRelease)
                      | otherwise =
                          let
                            !newCommissions = oldCommissions2 + commissionChange2
                            !changeRatio1 = TxRatio.unsafeRatio commissionChange1 oldCommissions1
                            !changeRelease1 = TxRatio.truncate (changeRatio1 Ptx.* TxRatio.fromInteger oldRelease1)
                            !newRelease = oldRelease2 - changeRelease1
                          in
                            (newCommissions, newRelease)
                in
                    input_FundHolding2_UTxO { 
                        LedgerApiV2.txOutDatum =
                                LedgerApiV2.OutputDatum $ FundHoldingT.mkDatum datumIn2 
                                {
                                    FundHoldingT.hdSubtotal_FT_Commissions = newCommissions2
                                    , FundHoldingT.hdSubtotal_FT_Commissions_Release_PerMonth_1e6 = newRelease2
                                }
                        , LedgerApiV2.txOutValue = output_FundHolding2_Value <> LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) commissionChange2
                    }
        -----------------
    in do
        mkContext
            |> setInputsRef [fund_UTxO_With_Added_FundHolding_MockData tp,
                        uTxOForValidatorAsReference tp (tpFundHoldingValidator tp)]
            |> setInputsAndAddRedeemers [(input_FundHolding1_UTxO, FundHoldingT.mkBalanceAssetsRedeemer commissionsFTAlterationsForRedeemer), (input_FundHolding2_UTxO, FundHoldingT.mkBalanceAssetsRedeemer commissionsFTAlterationsForRedeemer)]
            |> setOutputs [output_FundHolding1_UTxO, output_FundHolding2_UTxO]
            |> setSignatories (tpFundAdmins tp)
            |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------
