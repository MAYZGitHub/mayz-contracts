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
import           PlutusTx.Prelude                         (negate, (<>), head)

-- Project imports
import qualified Generic.OnChainHelpers                   as OnChainHelpers
import qualified Protocol.Fund.Helpers                    as FundHelpers
import qualified Protocol.Fund.Holding.Types              as FundHoldingT
import qualified Protocol.Fund.Types                      as FundT
import qualified Protocol.InvestUnit.Types                as InvestUnitT
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
        (userFT, commissionsFT, commissions_FT_Rate1e6_PerMonth) = calculateDepositCommissionsUsingMonths_Parametrizable tp input_Fund_Datum depositDate depositAmount
        --------------------
        investUnit_Value = OffChainHelpers.mkValue_From_InvestUnit_And_Amount2 input_InvestUnit depositAmount
        --------------------
        output_FundHolding_Datum = FundHelpers.mkUpdated_FundHolding_Datum_With_Deposit
                input_FundHolding_Datum
                depositAmount userFT commissionsFT commissions_FT_Rate1e6_PerMonth
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
            |> setMintAndAddRedeemers[ (LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) depositAmount, FundT.mkMintFTRedeemer)]
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
        (commissionsForUserFTToGetBack, withdrawPlusCommissionsGetBack, commissions_FT_Rate1e6_PerMonth) = calculateWithdrawCommissionsUsingMonths_Parametrizable tp input_Fund_Datum withdrawDate withdrawAmount investUnit_Granularity
        --------------------
        investUnit_Value = OffChainHelpers.mkValue_From_InvestUnit_And_Amount2 input_InvestUnit withdrawPlusCommissionsGetBack
        --------------------
        output_FundHolding_Datum = FundHelpers.mkUpdated_FundHolding_Datum_With_Withdraw
                input_FundHolding_Datum
                withdrawAmount commissionsForUserFTToGetBack commissions_FT_Rate1e6_PerMonth
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

fundHolding_BalanceAssets_TxContext :: TestParams -> [Integer] -> [Integer] -> [Integer] -> LedgerApiV2.ScriptContext
fundHolding_BalanceAssets_TxContext tp depositsInit depositsAfter redeemerCommissionsFT =
    let
        --------------------
        swTrace = False
        --------------------
        depositsInit_1 = P.head depositsInit
        depositsInit_2 = P.head (tail depositsInit)
        --------------------
        depositsAfter_1 = P.head depositsAfter
        depositsAfter_2 = P.head (tail depositsAfter)
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
        input_FundHolding1_Datum = FundHoldingT.getFundHolding_DatumType_From_UTxO input_FundHolding1_UTxO
        input_FundHolding1_Value = LedgerApiV2.txOutValue input_FundHolding1_UTxO
        -----------------
        input_FundHolding2_UTxO = fundHolding_UTxO_With_Deposits_MockData_Parametrizable tp input_Fund_Datum base_FundHolding_Datum input_InvestUnit 1 depositsInit_2 (tpDepositDate tp)
        input_FundHolding2_Datum = FundHoldingT.getFundHolding_DatumType_From_UTxO input_FundHolding2_UTxO
        input_FundHolding2_Value = LedgerApiV2.txOutValue input_FundHolding2_UTxO
        -----------------
        output_FundHolding1_UTxO = fundHolding_UTxO_With_Deposits_MockData_Parametrizable tp input_Fund_Datum base_FundHolding_Datum input_InvestUnit 0 depositsAfter_1 (tpDepositDate tp)
        output_FundHolding2_UTxO = fundHolding_UTxO_With_Deposits_MockData_Parametrizable tp input_Fund_Datum base_FundHolding_Datum input_InvestUnit 1 depositsAfter_2 (tpDepositDate tp)
        -- output_FundHolding1_Datum = input_FundHolding1_Datum
        -- output_FundHolding1_UTxO = input_FundHolding1_UTxO
        --     { LedgerApiV2.txOutDatum =
        --         LedgerApiV2.OutputDatum $
        --             FundHoldingT.mkDatum output_FundHolding1_Datum
        --     , LedgerApiV2.txOutValue = input_FundHolding1_Value
        --     }
        -- -----------------
        -- output_FundHolding2_Datum = input_FundHolding2_Datum
        -- output_FundHolding2_UTxO = input_FundHolding2_UTxO
        --     { LedgerApiV2.txOutDatum =
        --         LedgerApiV2.OutputDatum $
        --             FundHoldingT.mkDatum output_FundHolding2_Datum
        --     , LedgerApiV2.txOutValue = input_FundHolding2_Value
        --     }
        -----------------
    in do
        mkContext
            |> setInputsRef [fund_UTxO_With_Added_FundHolding_MockData tp,
                        uTxOForValidatorAsReference tp (tpFundHoldingValidator tp)]
            |> setInputsAndAddRedeemers [(input_FundHolding1_UTxO, FundHoldingT.mkBalanceAssetsRedeemer redeemerCommissionsFT), (input_FundHolding2_UTxO, FundHoldingT.mkBalanceAssetsRedeemer redeemerCommissionsFT)]
            |> setOutputs [output_FundHolding1_UTxO, output_FundHolding2_UTxO]
            |> setSignatories (tpFundAdmins tp)
            |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------
